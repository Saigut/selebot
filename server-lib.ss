(library (server-lib)
  (export hello-world)

  (import (chezscheme))

  (define hello-world
    (lambda ()
      (printf "Hello, World!~%")))



)


#|

(define buf-transcoder (make-transcoder (utf-8-codec) 'crlf 'replace))
(define header-buf (make-bytevector 4096 98))
(define binary-input-port (open-bytevector-input-port header-buf))
(define textual-input-port (open-bytevector-input-port (get-bytevector-n binary-input-port 10) buf-transcoder))
(get-line textual-input-port)


(define buf-transcoder (make-transcoder (utf-8-codec) 'crlf 'replace))

(define header-buf (string->bytevector "abcd\naaaaa\nvbbbbbbbb\nccccccccc\nddddddddd\n\n" buf-transcoder))

(define buf-input-port (open-bytevector-input-port header-buf buf-transcoder))

(define buf-input-port (open-bytevector-input-port header-buf))

|#
