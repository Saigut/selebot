(load "socket.ss")
(define client-sd (setup-client-socket "127.0.0.1" 6101))

(let ([str #f] [str-len 0] [sendout 0])
  (do ((break #f))
    (break
      (printf "Send Failed, Quit. Sent: ~a, Expected:~a. ~%" sendout str-len)
      -1)
    (set! str (get-line (current-input-port)))
    (set! str-len (string-length str))
    (set! sendout (c-write client-sd str str-len))
    (unless (= sendout str-len)
      (set! break #t))))
