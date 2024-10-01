(library (funcutils)
  (export constantly vector-for-each-truthy for-each-indexed
		  vector-index vector-popcnt)
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
	(do [(cur l (cdr cur))
		 (i 0 (add1 i))]
		[(null? cur)]
	  (f i (car cur))))

  (define (vector-index elem v)
	(define len (vector-length v))
	(let loop ([i 0])
	  (if (fx>= i len)
		  #f
		  (let ([elemi (vector-ref v i)])
			(if (eq? elem elemi)
				i
				(loop (add1 i)))))))

  (define (vector-popcnt v)
	(define len (vector-length v)) 
	(let loop ([i 0]
			   [n 0])
	  (if (fx>= i len)
		  n
		  (let ([elemi (vector-ref v i)])
			(if elemi
				(loop (add1 i) (add1 n))
				(loop (add1 i) n)))))))
