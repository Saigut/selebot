;;; if printf is not blocking, the code should blocking at these places
;;; so.. it seems that the blocking of printf(possibly all the print procedures) is the only possibility


(set! u8-list (append u8-list (list byte)))

(set! line (bytevector->string bv-line crlf-transcoder))

(printf "get 8~%")

(set! ret (eof-object))

(set! byte (get-u8 binary-input-port))

(not (eof-object? (lookahead-u8 binary-input-port)))

(set! u8-list (append u8-list (list byte)))

(not (eof-object? binary-header-line))

;;  loop variable assignment or break phase of (do (xxx) (xxx) xxx)

