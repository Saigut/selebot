(import	(chezscheme)
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

(define b-output-port (make-thread-parameter (standard-output-port 'none)))
(define b-input-port (make-thread-parameter (standard-input-port 'none)))

;;[b-output-port (standard-output-port 'none)]
;;[b-input-port (standard-input-port 'none)]

;;(define b-output-port (make-thread-parameter #f))
;;(define b-input-port (make-thread-parameter #f))

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

(define (b-pretty-print obj)
  ;;(pretty-print (b-output-port))
  (put-bytevector (b-output-port)
		  (call-with-bytevector-output-port
		   (lambda (p)
		     (pretty-print obj p))
		   none-transcoder))
  (flush-output-port (b-output-port)))

(define (b-printf . params)
  ;;(pretty-print (b-output-port))
  (put-bytevector (b-output-port)
		  (call-with-bytevector-output-port
		   (lambda (p)
		     (apply fprintf (cons p params)))
		   none-transcoder))
  (flush-output-port (b-output-port)))

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
	    ;;(b-printf "~a: carriage-return " (client-sd))
	    ;;(b-pretty-print (get-u8 binary-input-port))
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
    (b-printf "~a: want read ~a chars~%" (client-sd) n)
    (let ([readin (c-read socket bv start n)])
      #|(unless (> readin 0)
	      (conn-active #f))|#
      (b-printf "~a: read ~a chars~%" (client-sd) readin)
      readin)))

(define (make-w! socket)
  (lambda (bv start n)
    (b-printf "~a: want write ~a chars~%" (client-sd) n)
    (let ([sendout (c-write socket bv start n)])
	  #|(when (< sendout 0)
		(conn-active #f))|#
	  (b-printf "~a: wrote ~a chars~%" (client-sd) sendout)
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
  (b-printf "~a: response did~%" (client-sd)))

(define (response-html html port)
  (b-printf "in response-html 1~%")
  (set! body-len (string-length html))
  (put-bytevector port (string->bytevector (string-append
					      "HTTP/1.1 200 OK\r\n"
					      "Server: Selebot Server v0.01\r\n"
					      ;;"Data: " (date-str (gmt-date (current-seconds))) "\r\n"
					      "Content-Length:" " " (number->string body-len) "\r\n"
					      "Content-Type: text/html\r\n"
					      "\r\n"
					      html) none-transcoder))
  (b-printf "in response-html 2~%")
  (flush-output-port port)
  (b-printf "in response-html 3~%"))

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
    (b-printf "in cgi-get-abc~%")
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
  (b-printf "Dealing with Method OPTIONS~%")
  (response-404 port))

(define (deal-method-get request port)
  (b-printf "~a: Dealing with Method GET~%" (client-sd))

  (let* ([path (http-request-r-uri request)]
	 [file-path (string-append "./" path)])
    (b-pretty-print path)
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
      (b-pretty-print "to response 404")
      (response-404 port)])))

(define (deal-method-head request port)
  (b-printf "Dealing with Method HEAD~%")
  (response-404 port))

(define (deal-method-post request port)
  (b-printf "Dealing with Method POST~%")
  (response-404 port))

(define (deal-method-put request port)
  (b-printf "Dealing with Method PUT~%")
  (response-404 port))

(define (deal-method-delete request port)
  (b-printf "Dealing with Method DELETE~%")
  (response-404 port))

(define (deal-method-trace request port)
  (b-printf "Dealing with Method TRACE~%")
  (response-404 port))

(define (deal-method-connect request port)
  (b-printf "Dealing with Method CONNECT~%")
  (response-404 port))

(define (deal-method-unknown request port)
  (b-printf "Dealing with Method UNKNOWN~%")
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
		  (b-printf "~a: Unknown Method: ~s~%" (client-sd) method)
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

    (b-printf "~a: Here1~%" (client-sd))
    
    (set! binary-header-line (get-line-bytevector binary-input/output-port))
    (b-pretty-print binary-header-line)
    
    (b-printf "~a: Here2~%" (client-sd))

    (if (not (eof-object? binary-header-line))
	(let ()
	  (set! header-line (bytevector->string binary-header-line crlf-transcoder))
	  (b-printf "~a: Here2.1~%" (client-sd))
	  (b-printf header-line)
	  
	  (set! header-tokens (string-split header-line #\space))
	  (b-printf "~a: request line splited.~%" (client-sd))
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
		;;(b-printf "Secured path: ~s~%" (http-request-r-uri recv-request))
		(do ([bv-line #f]
		     [line #f]
		     [line-len 0]
		     [line-tokens #f]
		     [break #f])
		    (break)
		  (b-printf "~a: start to get the header line.~%" (client-sd))
		  (set! bv-line (get-line-bytevector binary-input/output-port))
		  (b-printf "~a: got header line.~%" (client-sd))
		  (if (not (eof-object? bv-line))
		      (let ()
			(b-printf "~a: not eof.~%" (client-sd))
			;;(b-printf "~a: " (client-sd))
			(b-pretty-print bv-line)
			(bytevector->string bv-line crlf-transcoder)
			(b-printf "~a: bytevector->string can do.~%" (client-sd))
			(set! line #f)
			(b-printf "~a: line set to false.~%" (client-sd))
			(set! line (bytevector->string bv-line crlf-transcoder))
			(b-printf "~a: bytevector->string did.~%" (client-sd))
			(set! line-len (string-length line))
			(b-printf "~a: set line-len.~%" (client-sd))
			(b-printf (string-append "~a: " line "~%") (client-sd))
			(set! line-tokens (string-split line #\: 2))
			(b-printf "~a: header line splited.~%" (client-sd))
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
				    (b-printf "~a: Malformed header.~%" (client-sd))
				    (set! ret #f)
				    (set! break #t))))))
		      (let ()
			(b-printf "~a: it's eof.~%" (client-sd))
			(set! ret #f)
			(set! break #t))))
		(b-printf "~a: deal headers finished.~%" (client-sd))

		(if ret
		    (deal-header recv-request binary-input/output-port))))
	(b-printf "~a: No request line.~%" (client-sd)))
    (close-port binary-input/output-port)))


;;; Deal with connection
(define client-conn
  (lambda ()
    ;;(current-output-port (transcoded-port (standard-output-port) none-transcoder))
    (b-output-port (standard-output-port 'none))
    (b-input-port (standard-input-port 'none))
    (do ()
	((not (conn-active))
	 (close (client-sd))
	 (b-printf "Client quit. sd: ~a~%~%" (client-sd)))
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
  (parameterize (
		 [current-input-port (standard-input-port 'none none-transcoder)]
		 )
		(client-sd (accept-socket (server-sd))))

  (if (> (client-sd) 0)
      (let ()
	(b-printf "New client connected. sd: ~a~%~%" (client-sd))
	(if (< (setsock-recvtimeout (client-sd) 2000) 0)
	    (let ()
	      (close (client-sd))
	      (b-printf "Client Closed due to something wrong.~%"))
	    (let ()
	      (parameterize
	       ([current-exception-state (create-exception-state default-exception-handler)]
		[current-output-port (transcoded-port (standard-output-port) none-transcoder)]
		[conn-active #t]
		;;[b-output-port (standard-output-port 'none)]
		;;[b-input-port (standard-input-port 'none)]
		)
			    (fork-thread client-conn))))
	(client-sd -1))))