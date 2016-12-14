(import (socket))

(define none-transcoder (make-transcoder (utf-8-codec) 'none 'replace))

(define client-sd (setup-client-socket "127.0.0.1" 6102))

(let ([str-bv #f] [bv-len 0] [sendout 0])
  (do ((break #f))
    (break
      (printf "Send Failed, Quit. Sent: ~a, Expected:~a. ~%" sendout bv-len)
      -1)
    (set! str-bv (string->bytevector (get-line (current-input-port)) none-transcoder))
    (set! bv-len (bytevector-length str-bv))
    (set! sendout (c-write client-sd str-bv 0 bv-len))
    (unless (= sendout bv-len)
      (set! break #t))))
