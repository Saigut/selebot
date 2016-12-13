(import	(chezscheme)
	(socket)
	(spells string-utils)
	(only (srfi srfi-13) string-index
	      string-trim-both)
	;;(srfi srfi-19)
	(server-lib))

;;(load "socket.ss")


(define-record-type http-request-r
  
  (fields
   (mutable method)
   (mutable uri)
   (mutable version)
   headers
   (mutable data)
   (mutable body))
  
  (nongenerative http-request-r-uuid-001))

(define-record-type http-conn-param-r
  
  (fields
   (mutable conn-active)
   (mutable begin-time)
   (mutable cur-time)
   (mutable keep-alive-time))
  
  (nongenerative http-conn-param-r-uuid-001))


;;; Global Variables
;; Transcoders
(define none-transcoder (make-transcoder (utf-8-codec) 'none 'replace))
(define crlf-transcoder (make-transcoder (utf-8-codec)
					 (eol-style crlf)
					 (error-handling-mode replace)))
(define def-keep-live-time 10)

;; Special characters
(define *line-feed* #x0a)
(define *carriage-return* #x0d)

;; Connection related
(define server-sd -1)
(define client-sd (make-thread-parameter -1))
(define conn-active (make-thread-parameter #f))
(define conn-param (make-thread-parameter #f))

;; CGIs
(define cgis (make-hashtable string-hash string=?))

;;; Utils

(define (f-pretty-print obj)
  (pretty-print obj)
  (flush-output-port (current-output-port)))

(define (f-printf . params)
  (apply printf params)
  (flush-output-port (current-output-port)))

(define (c-pretty-print obj)
  (ori-c-printf "in c-pretty-print 1\n")
  (ori-c-printf (call-with-string-output-port
		 (lambda (p)
		   (pretty-print obj p))))
  (ori-c-printf "in c-pretty-print 2\n"))

(define (c-printf . params)
  (ori-c-printf "in c-printf 1\n")
  (ori-c-printf (call-with-string-output-port
		 (lambda (p)
		   (apply fprintf (cons p params)))))
  (ori-c-printf "in c-printf 2\n"))

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
  (if (not (eof-object? (lookahead-u8 binary-input-port)))
      (let ()
	(do ([byte #f]
	     [break #f])
	    (break)
	  (set! byte (get-u8 binary-input-port))
	  (cond
	   [(or (eof-object? byte) (= byte *line-feed*))
	    (set! break #t)]
	   [(= byte *carriage-return*)
	    ;;(printf "~a: carriage-return " (client-sd))
	    (get-u8 binary-input-port)
	    (set! break #t)]
	   [else
	    (set! u8-list (append u8-list (list byte)))]))
	(set! ret (u8-list->bytevector u8-list)))
      (set! ret (eof-object)))
  ret)

(define (current-seconds)
  (time-second (current-time)))

(define (secure-path path)
  (define path-depth 0)
  
  (do ([inside-path path (path-rest inside-path)]
       [first-path ""]
       [ret-path ""]
       [break #f])
      (break ret-path)
    
    (set! first-path (path-first inside-path))
    
    (cond
     [(< path-depth 0)
      (set! path "/404.html")
      (set! break #t)]
     [(string=? first-path "/")]
     [(string=? first-path ".")]
     [(string=? first-path "..")
      (set! path-depth (- path-depth 1))]
     [(string=? first-path "")
      (set! break #t)]
     [else
      (set! path-depth (+ path-depth 1))]))

  path)



;;; Custom Port
(define (make-r! socket)
  (lambda (bv start n)
    ;;(printf "~a: want read ~a chars~%" (client-sd) n)
    (let ([readin (c-read socket bv start n)])
      #|(unless (> readin 0)
	      (conn-active #f))|#
      ;;(printf "~a: read ~a chars~%" (client-sd) readin)
      readin)))

(define (make-w! socket)
  (lambda (bv start n)
    ;;(printf "~a: want write ~a chars~%" (client-sd) n)
    (let ([sendout (c-write socket bv start n)])
	  #|(when (< sendout 0)
		(conn-active #f))|#
	  ;;(printf "~a: wrote ~a chars~%" (client-sd) sendout)
	  sendout)))

(define (make-close socket)
  (lambda ()
    (close socket)
    (http-conn-param-r-conn-active-set! (conn-param) #f)))


;;; Responses
(define (response-404 port)
  (define body "<h1>404. Page Not Found.</h1>")
  (define body-len (string-length body))
  (put-bytevector port (string->bytevector (string-append
					      "HTTP/1.1 404 Not Found\r\n"
					      "Server: Selebot Server v0.01\r\n"
					      ;;"Data: " (date-str (gmt-date (current-seconds))) "\r\n"
					      "Content-Length:" " " (number->string body-len) "\r\n"
					      "Content-Type: text/html\r\n"
					      "\r\n"
					      body) none-transcoder))
  (flush-output-port port)
  ;;(printf "~a: response did~%" (client-sd))
  )

(define (response-html html port)
  (define body-len (string-length html))
  (put-bytevector port (string->bytevector (string-append
					      "HTTP/1.1 200 OK\r\n"
					      "Server: Selebot Server v0.01\r\n"
					      ;;"Data: " (date-str (gmt-date (current-seconds))) "\r\n"
					      "Content-Length:" " " (number->string body-len) "\r\n"
					      "Content-Type: text/html\r\n"
					      "\r\n"
					      html) none-transcoder))
  (flush-output-port port))

(define (response-file file-port res-port)
  (let ([file-content (get-bytevector-all file-port)]
	[body-len 0])
    (close-port file-port)
    (if (not (eof-object? file-content))
	(set! body-len (bytevector-length file-content)))
    (put-bytevector res-port (string->bytevector (string-append
					      "HTTP/1.1 200 OK\r\n"
					      "Server: Selebot Server v0.01\r\n"
					      ;;"Data: " (date-str (gmt-date (current-seconds))) "\r\n"
					      "Content-Length:" " " (number->string body-len) "\r\n"
					      "Content-Type: text/html\r\n"
					      "\r\n") none-transcoder))
    (if (not (eof-object? file-content))
	(put-bytevector res-port file-content))
    
    (flush-output-port res-port)))


;;; Deal with CGIs
(define (cgi-get-exist? path)
  (let ([cgi-get (hashtable-ref cgis "GET" #f)])
    (if (not (boolean? cgi-get))
	(let ([deal-cgi-get (hashtable-ref cgi-get path #f)])
	  (if (not (boolean? deal-cgi-get))
	      deal-cgi-get
	      #f))
	#f)))

(define cgi-get-abc
  (lambda (resuest port)
    (response-html "<h1>You are /abc, Isn't you?</h1>" port)))


;;; Deal with files request
;;(define (file-exist? path)
;;  #f)
(define (deal-req-file request port)
  (let ([file-port (open-file-input-port
		    (string-append "./" (http-request-r-uri request))
		    (file-options no-create))])
    (response-file file-port port)))

;;; Deal with methods
(define (deal-method-options request port)
  ;;(printf "Dealing with Method OPTIONS~%")
  (response-404 port))

(define (deal-method-get request port)
  ;;(printf "~a: Dealing with Method GET~%" (client-sd))

  (let* ([path (http-request-r-uri request)]
	 [file-path (string-append "./" path)])
    (cond
     [(cgi-get-exist? path) => (lambda (deal-cgi-get) (deal-cgi-get request port))]
     [(file-exists? file-path)
      (cond
       [(file-regular? file-path)
	(deal-req-file request port)]
       [(file-directory? file-path)
	(response-404 port)]
       [else
	(response-404 port)])]
     [else
      ;;(printf "to response 404")
      (response-404 port)])))

(define (deal-method-head request port)
  ;;(printf "Dealing with Method HEAD~%")
  (response-404 port))

(define (deal-method-post request port)
  ;;(printf "Dealing with Method POST~%")
  (response-404 port))

(define (deal-method-put request port)
  ;;(printf "Dealing with Method PUT~%")
  (response-404 port))

(define (deal-method-delete request port)
  ;;(printf "Dealing with Method DELETE~%")
  (response-404 port))

(define (deal-method-trace request port)
  ;;(printf "Dealing with Method TRACE~%")
  (response-404 port))

(define (deal-method-connect request port)
  ;;(printf "Dealing with Method CONNECT~%")
  (response-404 port))

(define (deal-method-unknown request port)
  ;;(printf "Dealing with Method UNKNOWN~%")
  (response-404 port))


;;; Deal with request header
(define (deal-header request textual-port)

  (let ([method (http-request-r-method request)])
		(cond
		 [(string=? method "OPTIONS")
		  (deal-method-options request textual-port)]
		 
		 [(string=? method "GET")
		  (deal-method-get request textual-port)]
		 
		 [(string=? method "HEAD")
		  (deal-method-head request textual-port)]
		 
		 [(string=? method "POST")
		  (deal-method-post request textual-port)]
		 
		 [(string=? method "PUT")
		  (deal-method-put request textual-port)]
		 
		 [(string=? method "DELETE")
		  (deal-method-delete request textual-port)]
		 
		 [(string=? method "TRACE")
		  (deal-method-trace request textual-port)]
		 
		 [(string=? method "CONNECT")
		  (deal-method-connect request textual-port)]
		 
		 [else
		  ;;(printf "~a: Unknown Method: ~s~%" (client-sd) method)
		  (deal-method-unknown request textual-port)])))

;;; Deal with request, getting header
(define deal-req
  (lambda (recv-request binary-input/output-port)
    
    ;;(define binary-input/output-port (transcoded-port binary-input/output-port none-transcoder))

    (define binary-header-line #f)
    
    (define header-line #f)

    (define header-tokens #f)

    (define ret #f)

    #|(printf "time-difference: ")
    (pretty-print (time-difference
		  (http-conn-param-r-cur-time (conn-param))
		  (http-conn-param-r-begin-time (conn-param))))

    (printf "keep-alive-time: ")
    (pretty-print (http-conn-param-r-keep-alive-time (conn-param)))|#

    (if (time<=? (time-difference
		  (http-conn-param-r-cur-time (conn-param))
		  (http-conn-param-r-begin-time (conn-param)))
		 (http-conn-param-r-keep-alive-time (conn-param)))

	(let ()
	  (if (bytes-ready? (client-sd) 1000000)
	      (set! binary-header-line (get-line-bytevector binary-input/output-port))
	      (let ()
		(sleep (make-time 'time-duration 0 1))
		(set! binary-header-line (eof-object))))

	  (if (not (eof-object? binary-header-line))
	      (let ()
		(set! header-line (bytevector->string binary-header-line crlf-transcoder))
		;;(newline)
		;;(printf (string-append "~a: " header-line "~%") (client-sd))
		
		(set! header-tokens (string-split header-line #\space))
		(when (= (length header-tokens) 3)

		      (http-request-r-method-set! recv-request (list-ref header-tokens 0))

		      (http-request-r-version-set! recv-request (list-ref header-tokens 2))
		      (let* ([uri (list-ref header-tokens 1)] [idx (string-index uri #\?)])
			(if (number? idx)
			    (let ()
			      (http-request-r-uri-set! recv-request (secure-path (substring uri 0 idx)))
			      (http-request-r-data-set! recv-request (substring uri (+ idx 1) (string-length uri))))
			    (let ()
			      (http-request-r-uri-set! recv-request (secure-path uri))
			      (http-request-r-data-set! recv-request #f)
			      )))
		      (do ([bv-line #f]
			   [line #f]
			   [line-len 0]
			   [line-tokens #f]
			   [break #f])
			  (break)
			(set! bv-line (get-line-bytevector binary-input/output-port))
			(if (not (eof-object? bv-line))
			    (let ()
			      (set! line (bytevector->string bv-line crlf-transcoder))
			      (set! line-len (string-length line))
			      ;;(printf (string-append "~a: " line "~%") (client-sd))
			      (set! line-tokens (string-split line #\: 2))
			      (if (= 2 (length line-tokens))
				  (let ()
				    (hashtable-set! (http-request-r-headers recv-request)
						    (string-upcase (list-ref line-tokens 0))
						    (string-trim-both (list-ref line-tokens 1))))
				  (let ()
				    (if (string=? "" line)
					(let ()
					  (if (string-ci=? "keep-alive" (hashtable-ref (http-request-r-headers recv-request) (string-upcase "Connection") ""))
					      (http-conn-param-r-keep-alive-time-set! (conn-param)
										 (make-time 'time-duration 0 def-keep-live-time)))
					  
					  (set! ret #t)
					  (set! break #t))
					(let ()
					  ;;(printf "~a: Malformed header.~%" (client-sd))
					  (set! ret #f)
					  (set! break #t))))))
			    (let ()
			      ;;(printf "~a: it's eof.~%" (client-sd))
			      (set! ret #f)
			      (set! break #t))))
		      ;;(newline)
		      ;;(printf "~a: deal headers finished.~%" (client-sd))

		      (if ret
			  (let ()
			    (deal-header recv-request binary-input/output-port)
			    (http-conn-param-r-begin-time-set! (conn-param)
							     (current-time 'time-monotonic))))))
	      ;;(printf "~a: No request line.~%" (client-sd))
	      ))
	(close-port binary-input/output-port))
    
    (cond
     [(not (http-conn-param-r-conn-active (conn-param)))
      "quit"]
     [(string-ci=? "close" (hashtable-ref (http-request-r-headers recv-request) (string-upcase "Connection") ""))
      (close-port binary-input/output-port)]
     [(string-ci=? "keep-alive" (hashtable-ref (http-request-r-headers recv-request) (string-upcase "Connection") ""))
      (http-conn-param-r-cur-time-set! (conn-param)
				       (current-time 'time-monotonic))]
     [else
      (close-port binary-input/output-port)])))


;;; Deal with connection
(define client-conn
  (lambda ()
    ;;(current-output-port (transcoded-port (standard-output-port) none-transcoder))
    (define recv-request (make-http-request-r #f
					      #f
					      #f
					      (make-hashtable equal-hash equal?)
					      #f
					      #f))


    (define binary-input/output-port (make-custom-binary-input/output-port "network input port"
									   (make-r! (client-sd))
									   (make-w! (client-sd))
									   #f
									   #f
									   (make-close (client-sd))))

    
    (conn-param (make-http-conn-param-r #t
				   (current-time 'time-monotonic)
				   0
				   (make-time 'time-duration 0 0)))
    
    (http-conn-param-r-cur-time-set! (conn-param)
				     (http-conn-param-r-begin-time (conn-param)))

    (do ()
	((not (http-conn-param-r-conn-active (conn-param)))
	 ;;(close (client-sd))
	 ;;(printf "Client quit. sd: ~a~%~%" (client-sd))
	 )
      (deal-req recv-request binary-input/output-port))))

(set-stdout-null)

;;; Init CGIs
(hashtable-set! cgis "GET" (make-hashtable string-hash string=?))
(let ([cgi-get (hashtable-ref cgis "GET" #f)])
  (if (not (boolean? cgi-get))
      (hashtable-set! cgi-get "/abc" cgi-get-abc)))

;;; Set up server socket
(set! server-sd (setup-server-socket 6102))


;;; Loop for new connections
(do () (#f)
  #|(if (bytes-ready? server-sd 3000000)
      (let ()
	(printf "there is client come in.~%")
	(client-sd (accept server-sd)))
      (let ()
	(printf "no client come in.~%")
	"else"
	))|#

  (client-sd (accept server-sd))
  
  (if (> (client-sd) 0)
      (let ()
	;;(printf "New client connected. sd: ~a~%~%" (client-sd))
	(if (< (setsock-recvtimeout (client-sd) 2000) 0)
	    (let ()
	      (close (client-sd))
	      ;;(printf "Client Closed due to something wrong.~%")
	      )
	    (let ()
	      (parameterize
	       ([current-exception-state (create-exception-state default-exception-handler)]
		[current-output-port (transcoded-port (standard-output-port) none-transcoder)]
		[conn-active #t])
			    (fork-thread client-conn))))
	(client-sd -1))))
