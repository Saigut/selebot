(define (loop-print)
  (do ([i 0 (+ i 1)])
      (#f)
    (printf "count ~a~%" i)))

(do () (#f)
  (get-char (current-input-port))
  (fork-thread loop-print))
