(import (rnrs)
	(chezscheme)
	(spells string-utils)
	(only (srfi :13) string-index
	      string-trim-both)
	(srfi :19)
	(server-lib))

(load "socket.ss")

(define-record-type http-request-r
  
  (fields
   (mutable method)
   (mutable uri)
   (mutable version)
   headers
   (mutable data)
   (mutable body))
  
  (nongenerative http-request-r-uuid-001))

(define (current-seconds)
  (time-second (current-time)))

(define server-sd (make-parameter -1))
(define client-sd (make-thread-parameter -1))
(define conn-active (make-thread-parameter #f))


(define check
  ;; signal an error if status x is negative, using c-error to
  ;; obtain the operating-system's error message
  (lambda (who x)
    (if (< x 0)
        (error who (c-error))
        x)))

(define (make-r! socket)
  (lambda (bv start n)
    (let ([readin (c-read socket bv start n)])
      (unless (> readin 0)
	      (conn-active #f))
      readin)))

(define (make-w! socket)
  (lambda (bv start n)
    (printf "want write ~a chars~%" n)
    (let ([sendout (c-write socket bv start n)])
	  (when (< sendout 0)
		(conn-active #f))
	  (printf "wrote ~a chars~%" sendout)
	  sendout)))

(define (make-close socket)
  (lambda ()
    (close socket)
    (conn-active #f)))

(define (response-404 t-port)
  (put-string t-port (string-append "HTTP/1.1 404 Not Found\r\f"
				  "Server: selebot\r\f"
				  ;;"Data: " (date-str (gmt-date (current-seconds))) "\r\f"
				  "Content-Type: text/html; charset=utf-8
\r\f"))
  (flush-output-port t-port)
  (close-port t-port)
  (printf "response did~%"))

(define (deal-method-options request port)
  (printf "Dealing with Method OPTIONS~%")
  (response-404 port))

(define (deal-method-get request port)
  (printf "Dealing with Method GET~%")
  (response-404 port))

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

(define (deal-header request textual-port)

  (let ([method (http-request-r-method request)])
		(cond
		 [(string=? method "OPTIONS")
		  
		  (printf "~s~%" method)
		  (deal-method-options request textual-port)]
		 
		 [(string=? method "GET")
		  
		  (printf "~s~%" method)
		  (deal-method-get request textual-port)]
		 
		 [(string=? method "HEAD")
		  
		  (printf "~s~%" method)
		  (deal-method-head request textual-port)]
		 
		 [(string=? method "POST")
		  
		  (printf "~s~%" method)
		  (deal-method-post request textual-port)]
		 
		 [(string=? method "PUT")
		  
		  (printf "~s~%" method)
		  (deal-method-put request textual-port)]
		 
		 [(string=? method "DELETE")
		  
		  (printf "~s~%" method)
		  (deal-method-delete request textual-port)]
		 
		 [(string=? method "TRACE")
		  
		  (printf "~s~%" method)
		  (deal-method-trace request textual-port)]
		 
		 [(string=? method "CONNECT")
		  
		  (printf "~s~%" method)
		  (deal-method-connect request textual-port)]
		 
		 [else
		  (printf "Unknown Method: ~s~%" method)
		  (deal-method-unknown request textual-port)])))

(define deal-req
  (lambda (c-sd)
    (define recv-request (make-http-request-r #f
					      #f
					      #f
					      (make-hashtable equal-hash equal?)
					      #f
					      #f))
    (define buf-transcoder (make-transcoder (utf-8-codec) 'crlf 'raise))

    (define binary-input/output-port (make-custom-binary-input/output-port "network input port"
									   (make-r! c-sd)
									   (make-w! c-sd)
									   #f
									   #f
									   (make-close  c-sd)))
    (define textual-input/output-port (transcoded-port binary-input/output-port buf-transcoder))
    (define header-line #f)

    (define header-tokens #f)

    (define ret #f)

    
    (set! header-line (get-line textual-input/output-port))
    
    (if (not (eof-object? header-line))
	(let ()
	  
	  (pretty-print header-line)
	  
	  (set! header-tokens (string-split header-line #\space))
	  
	  (when (= (length header-tokens) 3)

		(http-request-r-method-set! recv-request (list-ref header-tokens 0))

		(http-request-r-version-set! recv-request (list-ref header-tokens 2))
		(let* ([uri (list-ref header-tokens 1)] [idx (string-index uri #\?)])
		  (if (number? idx)
		      (let ()
			(http-request-r-uri-set! recv-request (substring uri 0 idx))
			(http-request-r-data-set! recv-request (substring uri (+ idx 1) (string-length uri))))
		      (let ()
			(http-request-r-uri-set! recv-request uri)
			(http-request-r-data-set! recv-request #f)
			)))
		
		(do ([line (get-line textual-input/output-port)]
		     [line-len 0]
		     [line-tokens #f]
		     [break #f])
		    ((or (eof-object? line) break))
		  (set! line (get-line textual-input/output-port))
		  (set! line-len (string-length line))
		  (pretty-print line)
		  (set! line-tokens (string-split line #\: 2))
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
		(printf "~%")

		(if ret
		    (deal-header recv-request textual-input/output-port)))))))


(define client-conn
  (lambda ()
    (do ()
	((not (conn-active))
	 (printf "Client quit. sd: ~a~%~%" (client-sd)))
      (deal-req (client-sd)))))


(server-sd (setup-server-socket 6101))

(do () (#f)
  (client-sd (accept-socket (server-sd)))

  (if (> (client-sd) 0)
      (let ()
	(printf "New client connected. sd: ~a~%~%" (client-sd))
	(conn-active #t)
	(parameterize ([current-exception-state (create-exception-state default-exception-handler)])
		      (fork-thread client-conn)))))
