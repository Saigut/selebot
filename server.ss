(import (rnrs)
	(chezscheme)
	(spells string-utils)
	(server-lib))

(load "socket.ss")

(define-record-type http-request-r
  
  (fields
   (mutable method)
   (mutable uri)
   (mutable version)
   (mutable headers)
   (mutable body))
  
  (nongenerative http-request-r-uuid-001))


(define-record-type data-buffer-r
  
  (fields
   (mutable data-len)
   (mutable cur-pos)
   data)
  
  (nongenerative data-buffer-r-uuid-001))

(define recv-buffer (make-data-buffer-r 0 0 (make-bytevector 4096 0)))


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
    ;;(printf "make-r! want read: ~a~%" n)
    (let ([readin 0])
      (set! readin (c-read socket bv start n))
      (unless (> readin 0)
	      ;;(printf "Failed receive from network.~%")
	      (conn-active #f))
      ;;(printf "read in ~a byte~%" readin)
      readin)))

(define (make-r-old! socket)
  (lambda (bv start n)
    (printf "make-r! Mark 1.~%")
    (do ([want n]
	 [cur-start start]
	 [available 0]
	 [can-read 0]
	 [last-read 0]
	 [readin 0]
	 [break #f]
	 [ret-val 0])
	
	(break ret-val)
      (printf "make-r! Mark 2.~%")
      (set! want (- want last-read))
      (printf "want: ~a~%" want)

      (cond
       [(= want 0)
	(printf "make-r! Mark 3.~%")
	(set! break #t)
	(set! ret-val n)]
       [(> want 0)
	(printf "make-r! Mark 4.~%")
	(set! cur-start (+ cur-start last-read))

	(set! available (- (data-buffer-r-data-len recv-buffer)
			   (data-buffer-r-cur-pos recv-buffer)))
	
	(set! can-read (if (< want available)
			   want
			   available))
	
	(if (> can-read 0)
	    (let ()
	      (printf "make-r! Mark 4.1.~%")
	      (bytevector-copy! (data-buffer-r-data recv-buffer)
				(data-buffer-r-cur-pos recv-buffer)
				bv
				cur-start
				can-read)
	      
	      (set! last-read can-read)
	      (data-buffer-r-cur-pos-set! recv-buffer
					  (+
					   (data-buffer-r-cur-pos recv-buffer)
					   last-read)))
	    (let ()
	      (printf "make-r! Mark 4.2.~%")
	      (set! readin (c-read socket
				   (data-buffer-r-data recv-buffer)
				   0
				   (bytevector-length (data-buffer-r-data recv-buffer))))
	      (printf "make-r! Mark 4.3.~%")
	      (cond
	       [(> readin 0)
		(data-buffer-r-data-len-set! recv-buffer readin)
		(data-buffer-r-cur-pos-set! recv-buffer 0)
		(set! last-read 0)]
	       [else
		(set! break #t)
		(set! ret-val (- n want))
		(printf "Failed receive from network.~%")
		(conn-active #f)])))]
       [else
	(printf "make-r! Mark 5.~%")
	(set! break #t)
	(set! ret-val (- n want))
	(printf "Want number reduce to less 0.~%")]))))

(define (make-close socket)
  (lambda ()
    (check 'close (close socket))))

(define deal-req
  (lambda (c-sd)
    (define recv-request (make-http-request-r ""
					      ""
					      ""
					      (make-hashtable equal-hash equal?)
					      ""))
    (define buf-transcoder (make-transcoder (utf-8-codec) 'crlf 'replace))

    (define binary-input-port (make-custom-binary-input-port "network input port"
							     (make-r! c-sd)
							     #f
							     #f
							     (make-close  c-sd)))
    (define textual-input-port (transcoded-port binary-input-port buf-transcoder))
    (define header-line (get-line textual-input-port))

    (define tokens 0)


    
    
    (if (not (eof-object? header-line))
	(let ()
	  
	  (pretty-print header-line)
	  
	  (set! tokens (string-split header-line #\space))
	  
	  (when (= (length tokens) 3)
		(http-request-r-method-set! recv-request (list-ref tokens 0))
		(http-request-r-uri-set! recv-request (list-ref tokens 1))
		(http-request-r-version-set! recv-request (list-ref tokens 2))
		(do ([line (get-line textual-input-port) (get-line textual-input-port)]
		     [line-len 0]
		     [header-line #f]
		     [header-token #f]
		     [break #f])
		    ((or (eof-object? line) break))
		  (set! line-len (string-length line))
		  (set! header-line line)
		  (pretty-print header-line)
		  (set! header-token (string-split header-line #\: 2))
		  (if (= 2 (length header-token))
		      (hashtable-set! (http-request-r-headers recv-request)
				      (string-upcase (list-ref header-token 0))
				      (list-ref header-token 1))
		      (let ()
			(if (string= "" header-line)
			    (set! break #t)
			    (printf "Malformed header.~%")))))
		(printf "~%"))))))


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
