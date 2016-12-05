(define none-transcoder (make-transcoder (utf-8-codec) 'none 'replace))
(define thread-output-port (make-thread-parameter #f))

(define main-input-port (make-thread-parameter (standard-input-port 'none none-transcoder)))

(define main-output-port (make-thread-parameter #f))

(define client-sd (make-thread-parameter -1))



(define (loop-print)
  (do ([i 0 (+ i 1)])
      (#f)
    (format (main-output-port) "count ~a~%" i)))

;;(console-output-port (standard-output-port 'none none-transcoder))



(do () (#f)
  (get-char (current-input-port))
  (client-sd 1)
  (parameterize
   (
    [current-exception-state (create-exception-state default-exception-handler)]
    [main-output-port (standard-output-port 'none none-transcoder)]
    )
   (fork-thread loop-print)))
