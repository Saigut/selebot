
(define thread-name (make-thread-parameter "main thread"))

(define sleep-time (make-thread-parameter (make-time 'time-duration 200000000 0)))

(define (thread-print)
  (do () (#f)
    (printf "~s is printing something.~%" (thread-name))
    (sleep (sleep-time))))

(parameterize
 ([current-exception-state (create-exception-state default-exception-handler)]
  [thread-name "sub thread-1"]
  [sleep-time (make-time 'time-duration 210000000 0)])
 (fork-thread thread-print))

(parameterize
 ([current-exception-state (create-exception-state default-exception-handler)]
  [thread-name "sub thread-2"]
  [sleep-time (make-time 'time-duration 220000000 0)])
 (fork-thread thread-print))

(parameterize
 ([current-exception-state (create-exception-state default-exception-handler)]
  [thread-name "sub thread-3"]
  [sleep-time (make-time 'time-duration 230000000 0)])
 (fork-thread thread-print))

(parameterize
 ([current-exception-state (create-exception-state default-exception-handler)]
  [thread-name "sub thread-4"]
  [sleep-time (make-time 'time-duration 240000000 0)])
 (fork-thread thread-print))

(thread-print)
