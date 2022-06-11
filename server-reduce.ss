(import (chezscheme)
  (socket))






;;; Global Variables
;; Transcoders
(define none-transcoder (make-transcoder (utf-8-codec) 'none 'replace))
(define crlf-transcoder (make-transcoder (utf-8-codec)
                          (eol-style crlf)
                          (error-handling-mode replace)))

;; Special characters
(define *line-feed* #x0a)
(define *carriage-return* #x0d)

;; Connection related
(define server-sd -1)
(define client-sd (make-thread-parameter -1))


;;; Utils


(define (get-line-bytevector binary-input-port)
  ;;(define ret #f)
  ;;(define u8-list '())

  (let ([ret #f]
         [u8-list '()])

    (printf "in get-line-bytevector.~%")

    (if (not (eof-object? (lookahead-u8 binary-input-port)))
      (let ()
        (do ([byte #f]
              [break #f])
          (break)
          (set! byte (get-u8 binary-input-port))
          (cond
            [(or (eof-object? byte) (= byte *line-feed*))
              (printf "lf or eof")
              (set! break #t)]
            [(= byte *carriage-return*)
              (printf "cr~%")
              ;;(printf "~a: carriage-return " (client-sd))
              (get-u8 binary-input-port)
              (set! break #t)]
            [else
              (printf "data~%")
              (set! u8-list (append u8-list (list byte)))]))
        (set! ret (u8-list->bytevector u8-list)))
      (let ()
        (printf "nothing to read~%")
        (set! ret (eof-object))))
    ret))



;;; Custom Port
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
    (close socket)))


;;; Responses
(define (response-404 port)
  ;;(define body "<h1>404. Page Not Found.</h1>")
  ;;(define body-len (string-length body))
  (let* ([body "<h1>404. Page Not Found.</h1>"]
          [body-len (string-length body)])
    (put-bytevector port (string->bytevector (string-append
                                               "HTTP/1.1 404 Not Found\r\n"
                                               "Server: Selebot Server v0.01\r\n"
                                               ;;"Data: " (date-str (gmt-date (current-seconds))) "\r\n"
                                               "Content-Length:" " " (number->string body-len) "\r\n"
                                               "Content-Type: text/html\r\n"
                                               "\r\n"
                                               body) none-transcoder))
    (flush-output-port port)
    (printf "~a: response did~%" (client-sd))))




;;; Deal with request, getting header
(define deal-req
  (lambda (binary-input/output-port)

    ;;(define binary-input/output-port (transcoded-port binary-input/output-port none-transcoder))

    ;;(define binary-header-line #f)

    ;;(define header-line #f)

    ;;(define header-tokens #f)

    ;;(define ret #f)

    (let ([binary-header-line #f]
           [header-line #f]
           [header-tokens #f]
           [ret #f])


      (printf "in deal-req.~%")

      (set! binary-header-line (get-line-bytevector binary-input/output-port))
      ;;(set! binary-header-line (get-u8 binary-input/output-port))

      (if (not (eof-object? binary-header-line))
        (let ()
          (printf "read in something.~%")
          ;;(set! header-line (bytevector->string binary-header-line crlf-transcoder))
          (set! header-line "hehe")

          (printf (string-append "~a: " header-line "~%") (client-sd))
          (response-404 binary-input/output-port)
          ;;(put-bytevector binary-input/output-port
          ;;		  (string->bytevector "hi" none-transcoder))

        ))
      (close-port binary-input/output-port)

      (printf "deal-req finished.~%"))

  ))


;;; Deal with connection
(define client-conn
  (lambda ()

    (let ([binary-input/output-port
            (make-custom-binary-input/output-port
              "network input port"
              (make-r! (client-sd))
              (make-w! (client-sd))
              #f
              #f
              (make-close (client-sd)))])


      (deal-req binary-input/output-port))))


;;; Set up server socket
(set! server-sd (setup-server-socket 6102))


;;; Loop for new connections
(do () (#f)

  (client-sd (accept server-sd))

  (if (> (client-sd) 0)
    (let ()
      (printf "New client connected. sd: ~a~%~%" (client-sd))
      (parameterize
        ([current-exception-state (create-exception-state default-exception-handler)])
        (fork-thread client-conn))
      (client-sd -1))))
