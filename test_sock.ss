;; server
(load "socket.ss")
(define spath (tmpnam 0))
spath
(define sfd (setup-server-socket spath))
(define cfd (accept-socket sfd))
;(define ib (make-string 10 0)
(define ib (make-bytevector 10 0))
(c-read cfd ib 10)

#!eof

;; client
(load "socket.ss")
(define spath "<server-path>")
(define cfd (setup-client-socket spath))
(c-write cfd "1234567890" 10)

#!eof

;; test chez string
(load-shared-object "./test_chez.so")
(define mod-chez-str
  (foreign-procedure "mod_chez_str" (string integer-32)
		     void))
(define ib (make-string 10 #\0))
ib
(mod-chez-str ib 10)
ib

(define mod-chez-str
  (foreign-procedure "mod_chez_str" (u8* integer-32)
		     void))
(define ibb (make-bytevector 10 0))
ibb
(mod-chez-str ibb 10)
ibb
