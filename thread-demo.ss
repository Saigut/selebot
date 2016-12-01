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
(define (get-line-bytevector-old binary-input-port)
  (if (not (eof-object? (lookahead-u8 binary-input-port)))
      (u8-list->bytevector
       (let loop ()
	 (let ([byte (get-u8 binary-input-port)])
	   (cond
	    [(or (eof-object? byte) (= byte *line-feed*))
	     '()]
	    [(= byte *carriage-return*)
	     (get-u8 binary-input-port)
	     '()]
	    [else
	     (cons byte (loop))]))))
      (eof-object)))

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
    (printf "~a: want read ~a chars~%" (client-sd) n)
    (let ([readin (c-read socket bv start n)])
      (printf "~a: read ~a chars~%" (client-sd) readin)
      readin)))

(define (make-w! socket)
  (lambda (bv start n)
    (printf "~a: want write ~a chars~%" (client-sd) n)
    (let ([sendout (c-write socket bv start n)])
	  (printf "~a: wrote ~a chars~%" (client-sd) sendout)
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
  (printf "~a: response did~%" (client-sd)))

;;; Deal with request, getting header
(define deal-req
  (lambda (c-sd)
    (define binary-input/output-port (make-custom-binary-input/output-port "network input port"
									   (make-r! c-sd)
									   (make-w! c-sd)
									   #f
									   #f
									   (make-close  c-sd)))

    (define binary-line #f)
    
    (define line #f)
    
    (set! binary-line (get-line-bytevector binary-input/output-port))

    (if (not (eof-object? binary-line))
	(let ()
	  (set! line (bytevector->string binary-line none-transcoder))
	  (printf "~a: " (client-sd))
	  (pretty-print line)
	  (pretty-print "Here")
	  (if (string=? line "")
	      (let ()
		(pretty-print "Here1")		
		(response-404 binary-input/output-port)
		(close-port binary-input/output-port)))
	  (pretty-print "Here2"))
	(let ()
	  (pretty-print "Here3")
	  (response-404 binary-input/output-port)
	  (close-port binary-input/output-port)))))


;;; Deal with connection
(define client-conn
  (lambda ()
    (do ()
	((not (conn-active))
	 (printf "Client quit. sd: ~a~%~%" (client-sd)))
      (deal-req (client-sd)))))


;;; Set up server socket
(server-sd (setup-server-socket 6101))

;;; Loop for new connections
(do () (#f)
  (client-sd (accept-socket (server-sd)))

  (if (> (client-sd) 0)
      (let ()
	(printf "New client connected. sd: ~a~%~%" (client-sd))
	(if (< (setsock-recvtimeout (client-sd) 2000) 0)
	    (let ()
	      (close (client-sd))
	      (printf "Client Closed due to something wrong.~%"))
	    (let ()
	      (parameterize
	       ([current-exception-state (create-exception-state default-exception-handler)]
		[current-output-port (transcoded-port (standard-output-port) none-transcoder)]
		[conn-active #t])
			    (fork-thread client-conn))))
	(client-sd -1))))
