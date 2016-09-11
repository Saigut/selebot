;;; socket.ss
;;; R. Kent Dybvig May 1998
;;; Updated November 2005
;;; Public Domain
;;;
;;; bindings for socket operations and other items useful for writing
;;; programs that use sockets.

;;; Requires csocket.so, built from csocket.c.
(load-shared-object "./csocket.so")

;;; Requires from C library:
;;;   close, dup, execl, fork, kill, listen, tmpnam, unlink
(case (machine-type)
  [(i3le ti3le a6le ta6le) (load-shared-object "libc.so.6")]
  [(i3osx ti3osx) (load-shared-object "libc.dylib")]
  [else (load-shared-object "libc.so")])

;;; basic C-library stuff

(define close
  (foreign-procedure "close" (integer-32)
    integer-32))

(define dup
  (foreign-procedure "dup" (integer-32)
    integer-32))

(define execl4
  (let ((execl-help
         (foreign-procedure "execl"
           (string string string string integer-32)
           integer-32)))
    (lambda (s1 s2 s3 s4)
      (execl-help s1 s2 s3 s4 0))))

(define fork
  (foreign-procedure "fork" ()
    integer-32))

(define kill
  (foreign-procedure "kill" (integer-32 integer-32)
    integer-32))

(define listen
  (foreign-procedure "listen" (integer-32 integer-32)
    integer-32))

(define tmpnam
  (foreign-procedure "tmpnam" (integer-32)
    string))

(define unlink
  (foreign-procedure "unlink" (string)
    integer-32))

;;; routines defined in csocket.c

(define accept
  (foreign-procedure "do_accept" (integer-32)
    integer-32))

(define bytes-ready?
  (foreign-procedure "bytes_ready" (integer-32)
    boolean))

(define bind
  (foreign-procedure "do_bind" (integer-32 integer-32)
    integer-32))

(define c-error
  (foreign-procedure "get_error" ()
    string))

(define c-read
  (foreign-procedure "c_read" (integer-32 u8* integer-32)
    integer-32))

(define c-write
  (foreign-procedure "c_write" (integer-32 string integer-32)
    integer-32))

(define connect
  (foreign-procedure "do_connect" (integer-32 string integer-32)
    integer-32))

(define socket
  (foreign-procedure "do_socket" ()
    integer-32))

;;; higher-level routines

(define dodup
 ; (dodup old new) closes old and dups new, then checks to
 ; make sure that resulting fd is the same as old
  (lambda (old new)
    (check 'close (close old))
    (unless (= (dup new) old)
      (error 'dodup
        "couldn't set up child process io for fd ~s" old))))

(define dofork
 ; (dofork child parent) forks a child process and invokes child
 ; without arguments and parent with the child's pid
  (lambda (child parent)
    (let ([pid (fork)])
      (cond
        [(= pid 0) (child)]
        [(> pid 0) (parent pid)]
        [else (error 'fork (c-error))]))))

(define setup-server-socket
 ; create a socket, bind it to name, and listen for connections
  (lambda (port)
    (let ([sock (check 'socket (socket))])
      (check 'bind (bind sock port))
      (check 'listen (listen sock 100))
      sock)))

(define setup-client-socket
 ; create a socket and attempt to connect to server
  (lambda (ip port)
    (let ([sock (check 'socket (socket))])
      (check 'connect (connect sock ip port))
      sock)))

(define accept-socket
 ; accept a connection
  (lambda (sock)
    (check 'accept (accept sock))))

(define check
 ; signal an error if status x is negative, using c-error to
 ; obtain the operating-system's error message
  (lambda (who x)
    (if (< x 0)
        (error who (c-error))
        x)))

#!eof
