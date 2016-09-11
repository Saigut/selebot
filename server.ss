(load "socket.ss")
(define server-sd (setup-server-socket 6101))
(define client-sd (accept-socket server-sd))
(define buf (make-bytevector 40960 0))
(do () (#f)
  (let ([readin (c-read client-sd buf (bytevector-length buf))])
    (if (> readin 0)
	(let ([vec (make-bytevector readin)])
	  (bytevector-copy! buf 0 vec 0 readin)
	  (pretty-print (utf8->string vec))))))