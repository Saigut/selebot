(import (rnrs)
	(spells string-utils))

(load "socket.ss")

(define-record-type http-request-r
  (fields
   (mutable method)
   (mutable uri)
   (mutable version)
   (mutable headers)
   (mutable body))
  (nongenerative http-request-r-uuid-001))


(define server-sd (make-parameter (setup-server-socket 6101)))
(define client-sd (make-thread-parameter -1))


(define deal-req
  (lambda (str)
    (define sip (open-string-input-port str))
    (define recv-request (make-http-request-r ""
					      ""
					      ""
					      (make-hashtable equal-hash equal?)
					      ""))
    (define header-line (get-line sip))
    (define tokens (string-split header-line #\space))
    (pretty-print (substring header-line
			     0
			     (- (string-length header-line) 1)))
    (when (= (length tokens) 3)
	  (http-request-r-method-set! recv-request (list-ref tokens 0))
	  (http-request-r-uri-set! recv-request (list-ref tokens 1))
	  (http-request-r-version-set! recv-request (list-ref tokens 2))
	  (do ([line (get-line sip) (get-line sip)]
	       [line-len 0]
	       [header-line #f]
	       [header-token #f]
	       [break #f])
	      ((or (string=? "\r"  line) break))
	    (set! line-len (string-length line))
	    (set! header-line (substring line 0 (- line-len 1)))
	    (pretty-print header-line)
	    (set! header-token (string-split header-line #\: 2))
	    (if (= 2 (length header-token))
		(hashtable-set! (http-request-r-headers recv-request)
				(string-upcase (list-ref header-token 0))
				(list-ref header-token 1))
		(let ()
		  (printf "Invalid header.~%")
		  (set! break #t)))
	    )
	  (printf "~%"))))


(define client-conn
  (lambda ()
    (let ([buf (make-bytevector 40960 0)])
      (do ([break #f] [readin 0])
	  (break
	   (printf "Client quit. sd: ~a~%~%" (client-sd)))
        (set! readin (c-read (client-sd) buf (bytevector-length buf)))
        (if (> readin 0)
	    (let ([vec (make-bytevector readin)] [readin-str #f])
	      (bytevector-copy! buf 0 vec 0 readin)
	      (set! readin-str (utf8->string vec))
	      (deal-req readin-str))
	    (set! break #t))))))



(do () (#f)
  (client-sd (accept-socket (server-sd)))

  (if (> (client-sd) 0)
      (let ()
	(printf "New client connected. sd: ~a~%~%" (client-sd))
	(fork-thread client-conn))))






#!eof

(define buf (make-bytevector 40960 0))
(do () (#f)
  (let ([readin (c-read client-sd buf (bytevector-length buf))])
    (if (> readin 0)
	(let ([vec (make-bytevector readin)])
	  (bytevector-copy! buf 0 vec 0 readin)
	  (pretty-print (utf8->string vec))))))

(define factorial
  (lambda (n)
    (do ([i n (- i 1)] [a 1 (* a i)])
        ((zero? i) a))))
