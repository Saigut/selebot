(define *line-feed* #x0a)
(define *carriage-return* #x0d)

(define client-sd (make-thread-parameter 1))

(define sleep-time (make-thread-parameter (make-time 'time-duration 200000000 0)))

(define (f-printf . params)
  (apply printf params)
  (flush-output-port (current-output-port)))

(define (get-line-bytevector binary-input-port)
  ;;(define ret #f)
  ;;(define u8-list '())
  (let ([ret #f]
	[bv #f]
	[u8-list '()])
    (f-printf "~a: doing~%" (client-sd))
    (do ([byte #f]
	 [break #f])
	(break)
      (f-printf "~a: getting u8~%" (client-sd))
      (set! byte (get-u8 binary-input-port))
      ;;(set! byte *carriage-return*)
      (cond
       [(eof-object? byte)
	(f-printf "~a: no thread param get line~%" (client-sd))
	(f-printf "~a: eof-object~%" (client-sd))
	(set! ret (eof-object))
	(set! break #t)]
       [(= byte *line-feed*)
	(f-printf "~a: line-feed~%" (client-sd))
	(set! bv (u8-list->bytevector u8-list))
	(set! ret (make-bytevector (bytevector-length bv) 0))
	(bytevector-copy! bv 0 ret 0 (bytevector-length bv))
	(set! break #t)]
       [(= byte *carriage-return*)
	(f-printf "~a: carriage-return~%" (client-sd))
	;;(get-u8 binary-input-port)
	(set! bv (u8-list->bytevector u8-list))
	(set! ret (make-bytevector (bytevector-length bv) 0))
	(bytevector-copy! bv 0 ret 0 (bytevector-length bv))
	(set! break #t)]
       [else
	(f-printf "~a: else~%" (client-sd))
	(set! u8-list (append u8-list (list byte)))
	(f-printf "~a: appended~%" (client-sd))]))
    (set! bv #vu8(90))
    (set! ret (make-bytevector (bytevector-length bv) 0))
    (bytevector-copy! bv 0 ret 0 (bytevector-length bv))
    ret))

(define (get-line-loop)
  (do () (#f)
    (set! binary-line (get-line-bytevector (open-bytevector-input-port #vu8(91 93 94 92 90 13 10))))
    (sleep (sleep-time))))

(parameterize
 ([current-exception-state (create-exception-state default-exception-handler)]
  [client-sd 2]
  )
 (fork-thread get-line-loop))

(parameterize
 ([current-exception-state (create-exception-state default-exception-handler)]
  [client-sd 3]
  )
 (fork-thread get-line-loop))

(get-line-loop)
