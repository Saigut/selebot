3;;; socket.ss
;;; R. Kent Dybvig May 1998
;;; Updated November 2005
;;; Public Domain
;;;
;;; bindings for socket operations and other items useful for writing
;;; programs that use sockets.

;;; Requires csocket.so, built from csocket.c.
(library (socket)
  (export close
	  dup
	  execl4
	  fork
	  kill
	  listen
	  tmpnam
	  unlink
	  accept
	  bytes-ready?
	  bind
	  c-error
	  c-read
	  c-write
	  connect
	  socket
	  setsock-recvtimeout
	  ori-c-printf
	  set-stdout-null
	  dodup
	  dofork
	  setup-server-socket
	  setup-client-socket
	  accept-socket
	  check)
  (import (chezscheme))


  (define csocket.so
    (load-shared-object "./csocket.so"))

    ;; Requires from C library:
    ;;   close, dup, execl, fork, kill, listen, tmpnam, unlink
  (define libc.so
    (case (machine-type)
      [(i3le ti3le a6le ta6le) (load-shared-object "libc.so.6")]
      [(i3osx ti3osx) (load-shared-object "libc.dylib")]
      [else (load-shared-object "libc.so")]))

;;; basic C-library stuff

(define close
  (foreign-procedure "close" (int)
    int))

(define dup
  (foreign-procedure "dup" (int)
    int))

(define execl4
  (let ((execl-help
         (foreign-procedure "execl"
           (string string string string int)
           int)))
    (lambda (s1 s2 s3 s4)
      (execl-help s1 s2 s3 s4 0))))

(define fork
  (foreign-procedure "fork" ()
    int))

(define kill
  (foreign-procedure "kill" (int int)
    int))

(define listen
  (foreign-procedure "listen" (int int)
    int))

(define tmpnam
  (foreign-procedure "tmpnam" (int)
    string))

(define unlink
  (foreign-procedure "unlink" (string)
    int))

;;; routines defined in csocket.c

(define accept
  (foreign-procedure "do_accept" (int)
    int))

(define bytes-ready?
  (foreign-procedure "bytes_ready" (int int)
    boolean))

(define bind
  (foreign-procedure "do_bind" (int int)
    int))

(define c-error
  (foreign-procedure "get_error" ()
    string))

(define c-read
  (foreign-procedure "c_read" (int u8* size_t size_t)
    ssize_t))

(define c-write
  (foreign-procedure "c_write" (int u8* size_t ssize_t)
    ssize_t))

(define connect
  (foreign-procedure "do_connect" (int string int)
    int))

(define socket
  (foreign-procedure "do_socket" ()
    int))

(define setsock-recvtimeout
   (foreign-procedure "setsock_recvtimeout" (int int)
    int))

(define ori-c-printf
  (foreign-procedure "printf" (string)
		     int))

(define set-stdout-null
  (foreign-procedure "set_stdout_null" ()
		     void))


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

)
