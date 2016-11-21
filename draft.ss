(define (f)
  (printf "I am in subthread.~%")
  (if (#f)
      1
      2))

(fork-thread f)
