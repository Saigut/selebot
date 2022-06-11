(import (socket))

(define none-transcoder (make-transcoder (utf-8-codec) 'none 'replace))

(define client-sd -1)

(define b-i/o-port #f)

(define (make-r! socket)
  (lambda (bv start n)
    (let ([readin (c-read socket bv start n)])
      readin)))

(define (make-w! socket)
  (lambda (bv start n)
    (let ([sendout (c-write socket bv start n)])
      sendout)))

(define (make-close socket)
  (lambda ()
    (close socket)
  ))


(let ()
  (do ([count 0] [break #f])
    (break)

    (set! client-sd (setup-client-socket "127.0.0.1" 6102))

    (if (> client-sd 0)

      (let ()

        (set! count (+ count 1))

        (set! b-i/o-port (make-custom-binary-input/output-port "network input port"
                           (make-r! client-sd)
                           (make-w! client-sd)
                           #f
                           #f
                           (make-close client-sd)))

        (put-bytevector b-i/o-port (string->bytevector
                                     "GET /abc HTTP/1.1\r\n\r\n"
                                     none-transcoder))

        (get-u8 b-i/o-port)

        (close-port b-i/o-port)
        (printf "Closed. ~a~%" count)))))
