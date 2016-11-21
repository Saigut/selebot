(import (rnrs)
	(chezscheme)
	(spells string-utils)
	(only (srfi :13) string-index
                string-trim-both)
	(server-lib))

;;(define (f fuck) (if (fuck) 1 2))

(define f
  (lambda ()
    (define sth #f)

    (if #t
	(let ()
	  (when #t
		(if (#f)
		    1
		    2))))))


(define u
  (lambda ()
    (do () (#f)
	(f))))

(do () (#f)
  (u))
