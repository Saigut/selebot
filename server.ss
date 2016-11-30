(import (rnrs)
	(chezscheme)
	;;(socket)
	(spells string-utils)
	(only (srfi :13) string-index
	      string-trim-both)
	(srfi :19)
	(server-lib))

(load "./socket.ss")

(define-record-type http-request-r
  
  (fields
   (mutable method)
   (mutable uri)
   (mutable version)
   headers
   (mutable data)
   (mutable body))
  
  (nongenerative http-request-r-uuid-001))

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

;; CGIs
(define cgis (make-hashtable string-hash string=?))

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
	    (printf "carriage-return")
	    (pretty-print (get-u8 binary-input-port))
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
    (printf "want read ~a chars~%" n)
    (let ([readin (c-read socket bv start n)])
      #|(unless (> readin 0)
	      (conn-active #f))|#
      (printf "read ~a chars~%" readin)
      readin)))

(define (make-w! socket)
  (lambda (bv start n)
    (printf "want write ~a chars~%" n)
    (let ([sendout (c-write socket bv start n)])
	  #|(when (< sendout 0)
		(conn-active #f))|#
	  (printf "wrote ~a chars~%" sendout)
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
					      ;;"Data: " (date-str (gmt-date (current-seconds))) "\r\n"
					      "Content-Length:" " " (number->string body-len) "\r\n"
					      "Content-Type: text/html\r\n"
					      "\r\n"
					      body) none-transcoder))
  (flush-output-port port)
  (printf "response did~%"))

(define (response-html html port)
  (set! body-len (string-length html))
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
  (printf "Dealing with Method OPTIONS~%")
  (response-404 port))

(define (deal-method-get request port)
  (printf "Dealing with Method GET~%")

  (let* ([path (http-request-r-uri request)]
	 [file-path (string-append "./" path)])
    (pretty-print path)
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
      (response-404 port)])))

(define (deal-method-head request port)
  (printf "Dealing with Method HEAD~%")
  (response-404 port))

(define (deal-method-post request port)
  (printf "Dealing with Method POST~%")
  (response-404 port))

(define (deal-method-put request port)
  (printf "Dealing with Method PUT~%")
  (response-404 port))

(define (deal-method-delete request port)
  (printf "Dealing with Method DELETE~%")
  (response-404 port))

(define (deal-method-trace request port)
  (printf "Dealing with Method TRACE~%")
  (response-404 port))

(define (deal-method-connect request port)
  (printf "Dealing with Method CONNECT~%")
  (response-404 port))

(define (deal-method-unknown request port)
  (printf "Dealing with Method UNKNOWN~%")
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
		  (printf "Unknown Method: ~s~%" method)
		  (deal-method-unknown request textual-port)])))

;;; Deal with request, getting header
(define deal-req
  (lambda (c-sd)
    (define recv-request (make-http-request-r #f
					      #f
					      #f
					      (make-hashtable equal-hash equal?)
					      #f
					      #f))


    (define binary-input/output-port (make-custom-binary-input/output-port "network input port"
									   (make-r! c-sd)
									   (make-w! c-sd)
									   #f
									   #f
									   (make-close  c-sd)))
    ;;(define binary-input/output-port (transcoded-port binary-input/output-port none-transcoder))

    (define binary-header-line #f)
    
    (define header-line #f)

    (define header-tokens #f)

    (define ret #f)

    (pretty-print "Here1")
    
    (set! binary-header-line (get-line-bytevector binary-input/output-port))

    (pretty-print "Here2")

    (if (not (eof-object? binary-header-line))
	(let ()
	  (set! header-line (bytevector->string binary-header-line crlf-transcoder))
	  (pretty-print "Here2.1")
	  (pretty-print header-line)
	  
	  (set! header-tokens (string-split header-line #\space))
	  (pretty-print "request line splited.")
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
		;;(printf "Secured path: ~s~%" (http-request-r-uri recv-request))
		(do ([bv-line #f]
		     [line #f]
		     [line-len 0]
		     [line-tokens #f]
		     [break #f])
		    (break)
		  (pretty-print "start to get the header line.")
		  (set! bv-line (get-line-bytevector binary-input/output-port))
		  (pretty-print "got header line.")
		  (if (not (eof-object? bv-line))
		      (let ()
			(pretty-print "not eof.")
			(pretty-print bv-line)
			(bytevector->string bv-line crlf-transcoder)
			(pretty-print "bytevector->string can do.")
			(set! line #f)
			(pretty-print "line set to false.")
			(set! line (bytevector->string bv-line crlf-transcoder))
			(pretty-print "bytevector->string did.")
			(set! line-len (string-length line))
			(pretty-print "set line-len.")
			(pretty-print line)
			(set! line-tokens (string-split line #\: 2))
			(pretty-print "header line splited.")
			(if (= 2 (length line-tokens))
			    (let ()
			      (hashtable-set! (http-request-r-headers recv-request)
					      (string-upcase (list-ref line-tokens 0))
					      (list-ref line-tokens 1)))
			    (let ()
			      (if (string=? "" line)
				  (let ()
				    (set! ret #t)
				    (set! break #t))
				  (let ()
				    (printf "Malformed header.~%")
				    (set! ret #f)
				    (set! break #t))))))
		      (let ()
			(pretty-print "it's eof.")
			(set! ret #f)
			(set! break #t))))
		(printf "deal headers finished.~%")

		(if ret
		    (deal-header recv-request binary-input/output-port))))
	(pretty-print "No request line."))
    (close-port binary-input/output-port)))


;;; Deal with connection
(define client-conn
  (lambda ()
    (do ()
	((not (conn-active))
	 (printf "Client quit. sd: ~a~%~%" (client-sd)))
      (deal-req (client-sd)))))

;;; Init CGIs
(hashtable-set! cgis "GET" (make-hashtable string-hash string=?))
(let ([cgi-get (hashtable-ref cgis "GET" #f)])
  (if (not (boolean? cgi-get))
      (hashtable-set! cgi-get "/abc" cgi-get-abc)))

;;; Set up server socket
(server-sd (setup-server-socket 6102))

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
	    (let () (conn-active #t)
		 (parameterize ([current-exception-state (create-exception-state default-exception-handler)])
			       (fork-thread client-conn)))))))
