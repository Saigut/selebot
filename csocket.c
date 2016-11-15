/*/ csocket.c
R. Kent Dybvig May 1998
Public Domain
/*/

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <stdio.h>
#include <unistd.h>

/* c_write attempts to write the entire buffer, pushing through
   interrupts, socket delays, and partial-buffer writes */
int c_write(int fd, char *buf, ssize_t start, ssize_t n) {
    ssize_t i, m;

    buf += start;
    m = n;
    while (m > 0) {
        if ((i = write(fd, buf, m)) < 0) {
            if (errno != EAGAIN && errno != EINTR)
                return i;
        } else {
            m -= i;
            buf += i;
        }
    }
    return n;
}

/* c_read pushes through interrupts and socket delays */
int c_read(int fd, char *buf, size_t start, size_t n) {
    int i;

    buf += start;
    for (;;) {
        i = read(fd, buf, n);
        if (i >= 0) return i;
        if (errno != EAGAIN && errno != EINTR) return -1;
    }
}

/* bytes_ready(fd) returns true if there are bytes available
   to be read from the socket identified by fd */
int bytes_ready(int fd) {
    int n;

    (void) ioctl(fd, FIONREAD, &n);
    return n;
}

/* socket support */

/* do_socket() creates a new AF_UNIX socket */
int do_socket(void) {

    return socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
}

/* do_bind(s, name) binds name to the socket s */
int do_bind(int s, int port) {
    int on = 1;
    
    if((setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on))) < 0) {  
	return -1;
    }
    
    struct sockaddr_in sin;

    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = htonl(INADDR_ANY);
    sin.sin_port = htons(port);

    return bind(s, (struct sockaddr *)&sin, sizeof(sin));
}

/* do_accept accepts a connection on socket s */
int do_accept(int s) {
     return accept(s, NULL, NULL);
}

/* do_connect initiates a socket connection */
int do_connect(int s, char *ip, int port) {
    struct sockaddr_in sin;

    sin.sin_family = AF_INET;
    if (inet_pton(AF_INET, ip, &sin.sin_addr) != 1) return -1;
    sin.sin_port = htons(port);

    return connect(s, (struct sockaddr *)&sin, sizeof(sin));
}

/* get_error returns the operating system's error status */
char* get_error(void) {
    extern int errno;
    return strerror(errno);
}
