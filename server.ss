(load "socket.ss")

(define server-sd (setup-server-socket 6101))
(define client-sd (make-thread-parameter -1))


(define deal-req
  (lambda (str)
    (define sip (open-string-input-port str))
    (define line)
    (do ([token (get-line sip) (get-line sip)]
	 [token-len 0])
      ((eof-object? token))
      (set! token-len (string-length token))
      (set! line (substring token 0 (- token-len 1)))
      (pretty-print line))))


(define client-conn
  (lambda ()
    (let ([buf (make-bytevector 40960 0)])
      (do ([break #f] [readin 0])
        (break
          (printf "Client quit. sd: ~a~%" client-sd))
        (set! readin (c-read client-sd buf (bytevector-length buf)))
        (if (> readin 0)
          (let ([vec (make-bytevector readin)] [readin-str #f])
            (bytevector-copy! buf 0 vec 0 readin)
            (set! readin-str (utf8->string vec))
            (deal-req readin-str))
          (set! break #t))))))



(do () (#f)
  (set! client-sd (accept-socket server-sd))

  (if (> client-sd 0)
    (let ()
      (printf "New client connected. sd: ~a~%" client-sd)
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
