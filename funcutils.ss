(library (funcutils)
  (export constantly vector-for-each-truthy for-each-indexed)
  (import (chezscheme))
  
  (define constantly
	;; some common stuff to avoid gc spamming
	(let* ([constantly-t (lambda _args #t)])
	  (lambda (x)
		(cond
		 [(eq? #t x) constantly-t]
		 [else (lambda _args x)]))))

  (define (vector-for-each-truthy f v)
	(vector-for-each
	 (lambda (e) (when e (f e)))
	 v))

  (define (for-each-indexed f l)
	(unless (null? l)
	  (do [(cur l (cdr cur))
		   (i 0 (add1 i))]
		  [(null? cur)]
		(f i (car cur))))))
