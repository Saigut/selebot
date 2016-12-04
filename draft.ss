(define (f)
  (printf "I am in subthread.~%")
  (if (#f)
      1
      2))

(fork-thread f)


;;; http://paste.lisp.org/display/332414#1
(define (sleep-sec s)
  (sleep (make-time 'time-duration 0 s)))

(begin (fork-thread new-cafe)
       (let loop ()
	  (printf "id:~d~n" (get-thread-id))
	   (sleep-sec 3)
	    (loop)))

(define x 0)
(define (sleep-sec s)
  (sleep (make-time 'time-duration 0 s)))

(begin (fork-thread new-cafe)
       (let loop ()
	  (printf "id:~d x:~d~n" (get-thread-id) x)
	   (sleep-sec 3)
	    (loop)))
