(load "socket.ss")

(define server-sd (setup-server-socket 6101))
(define client-sd (make-thread-parameter -1))

(define deal-req
  (lambda (str))

(define client-conn
  (lambda ()
    (let ([buf #f] [readin 0] [vec 0] [readin-str #f])
      (set! buf (make-bytevector 40960 0))
      (do ((break #f))
        (break
          (printf "Client quit. sd: ~a~%" client-sd)
          0)
        (set! readin (c-read client-sd buf (bytevector-length buf)))
        (if (> readin 0)
          (let ()
            (set! vec (make-bytevector readin))
            (bytevector-copy! buf 0 vec 0 readin)
            (set! readin-str (utf8->string vec))
            (pretty-print readin-str))
            (deal-req readin-str)
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
