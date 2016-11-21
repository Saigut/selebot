(import (rnrs)
	(chezscheme)
	(spells string-utils)
	(only (srfi :13) string-index
                string-trim-both)
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

(define (make-close socket)
  (lambda ()
    (check 'close (close socket))))

(define (deal-header request)
  (if (string-ci=? (http-request-r-method request) "get")
      (printf "GET~%")
      (printf "Not GET~%")))

(define deal-req
  (lambda (c-sd)
    (define recv-request (make-http-request-r #f
					      #f
					      #f
					      (make-hashtable equal-hash equal?)
					      #f
					      #f))
    (define buf-transcoder (make-transcoder (utf-8-codec) 'crlf 'raise))

    (define binary-input-port (make-custom-binary-input-port "network input port"
							     (make-r! c-sd)
							     #f
							     #f
							     (make-close  c-sd)))
    (define textual-input-port (transcoded-port binary-input-port buf-transcoder))
    (define header-line (get-line textual-input-port))

    (define header-tokens #f)

    (define ret #f)

    
    
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
		
		(do ([line (get-line textual-input-port)]
		     [line-len 0]
		     [line-tokens #f]
		     [break #f])
		    ((or (eof-object? line) break))
		  (set! line (get-line textual-input-port))
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
		    (deal-header recv-request)))))))


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
	(fork-thread client-conn))))
