(define none-transcoder (make-transcoder (utf-8-codec) 'none 'replace))
(define thread-name (make-thread-parameter "main thread"))

(define sleep-time (make-thread-parameter (make-time 'time-duration 2000000 0)))

(define (thread-print)
  (do () (#f)
    (printf "~s is printing something.~%" (thread-name))
    (sleep (sleep-time))))

(parameterize
 ([current-exception-state (create-exception-state default-exception-handler)]
  [thread-name "sub thread-1"]
  [sleep-time (make-time 'time-duration 2100000 0)]
  [current-output-port (transcoded-port (standard-output-port) none-transcoder)])
 (fork-thread thread-print))

(parameterize
 ([current-exception-state (create-exception-state default-exception-handler)]
  [thread-name "sub thread-2"]
  [sleep-time (make-time 'time-duration 2200000 0)]
  [current-output-port (transcoded-port (standard-output-port) none-transcoder)])
 (fork-thread thread-print))

(parameterize
 ([current-exception-state (create-exception-state default-exception-handler)]
  [thread-name "sub thread-3"]
  [sleep-time (make-time 'time-duration 2300000 0)]
  [current-output-port (transcoded-port (standard-output-port) none-transcoder)])
 (fork-thread thread-print))

(parameterize
 ([current-exception-state (create-exception-state default-exception-handler)]
  [thread-name "sub thread-4"]
  [sleep-time (make-time 'time-duration 2400000 0)]
  [current-output-port (transcoded-port (standard-output-port) none-transcoder)])
 (fork-thread thread-print))

(thread-print)
