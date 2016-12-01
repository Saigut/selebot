(import	(chezscheme))

(load "./socket.ss")

;;; Global Variables
;; Transcoders
(define none-transcoder (make-transcoder (utf-8-codec) 'none 'replace))
(define crlf-transcoder (make-transcoder (utf-8-codec)
					 (eol-style crlf)
					 (error-handling-mode replace)))

;; Special characters
(define *line-feed* #x0a)
(define *carriage-return* #x0d)

;; Connection related
(define server-sd (make-parameter -1))
(define client-sd (make-thread-parameter -1))
(define conn-active (make-thread-parameter #f))

;;; Utils
(define (f-pretty-print obj)
  (pretty-print obj)
  (flush-output-port (current-output-port)))

(define (f-printf . params)
  (apply printf params)
  (flush-output-port (current-output-port)))

(define (get-line-bytevector binary-input-port)
  (define ret #f)
  (define u8-list '())
  (do ([byte #f]
       [break #f])
      (break)
    (set! byte (get-u8 binary-input-port))
    (cond
     [(eof-object? byte)
      (set! ret (eof-object))
      (set! break #t)]
     [(= byte *line-feed*)
      (set! ret (u8-list->bytevector u8-list))
      (set! break #t)]
     [(= byte *carriage-return*)
      (get-u8 binary-input-port)
      (set! ret (u8-list->bytevector u8-list))
      (set! break #t)]
     [else
      (set! u8-list (append u8-list (list byte)))]))
  ret)

;;; Custom Port
(define (make-r! socket)
  (lambda (bv start n)
    ;;(f-printf "~a: want read ~a chars~%" (client-sd) n)
    (let ([readin (c-read socket bv start n)])
      ;;(f-printf "~a: read ~a chars~%" (client-sd) readin)
      readin)))

(define (make-w! socket)
  (lambda (bv start n)
    ;;(f-printf "~a: want write ~a chars~%" (client-sd) n)
    (let ([sendout (c-write socket bv start n)])
	  ;;(f-printf "~a: wrote ~a chars~%" (client-sd) sendout)
	  sendout)))

(define (make-close socket)
  (lambda ()
    (close socket)
    (conn-active #f)))


;;; Responses
(define (response-404 port)
  (set! body "<h1>404. Page Not Found.</h1>")
  (set! body-len (string-length body))
  (put-bytevector port (string->bytevector (string-append
					      "HTTP/1.1 404 Not Found\r\n"
					      "Server: Selebot Server v0.01\r\n"

					      "Content-Length:" " " (number->string body-len) "\r\n"
					      "Content-Type: text/html\r\n"
					      "\r\n"
					      body) none-transcoder))
  (flush-output-port port)
  (f-printf "~a: response did~%" (client-sd)))


;;; Deal with request, getting line
(define deal-line
  (lambda (binary-input/output-port)
    

    (define binary-line #f)
    
    (define line #f)

    (f-printf "~a: getting line~%" (client-sd))
    
    (set! binary-line (get-line-bytevector binary-input/output-port))

    (f-printf "~a: got line~%" (client-sd))

    (if (not (eof-object? binary-line))
	(let ()
	  (f-printf "~a: do bytevector->string~%" (client-sd))
	  (set! line (bytevector->string binary-line none-transcoder))
	  (f-printf "~a: " (client-sd))
	  (f-printf line)
	  (f-printf "~%~a: printed line~%" (client-sd))
	  (if (string=? line "")
	      (let ()
		(response-404 binary-input/output-port)
		(close-port binary-input/output-port))))
	(let ()
	  (response-404 binary-input/output-port)
	  (close-port binary-input/output-port)))
    (f-printf "~a: dealed line~%" (client-sd))))


;;; Deal with connection
(define client-conn
  (lambda ()
    (define binary-input/output-port (make-custom-binary-input/output-port "network input port"
									   (make-r! (client-sd))
									   (make-w! (client-sd))
									   #f
									   #f
									   (make-close (client-sd))))
    (do ()
	((not (conn-active))
	 (f-printf "Client quit. sd: ~a~%~%" (client-sd)))
      (deal-line binary-input/output-port))))


;;; Set up server socket
(server-sd (setup-server-socket 6101))

;;; Loop for new connections
(do () (#f)
  (client-sd (accept-socket (server-sd)))

  (if (> (client-sd) 0)
      (let ()
	(f-printf "New client connected. sd: ~a~%~%" (client-sd))
	(if (< (setsock-recvtimeout (client-sd) 2000) 0)
	    (let ()
	      (close (client-sd))
	      (f-printf "Client Closed due to something wrong.~%"))
	    (let ()
	      (parameterize
	       ([current-exception-state (create-exception-state default-exception-handler)]
		[current-output-port (transcoded-port (standard-output-port) none-transcoder)]
		[conn-active #t])
			    (fork-thread client-conn))))
	(client-sd -1))))
