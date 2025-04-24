;; Copyright (C) 2025 Vincent Lee; GPL-3.0-or-later
(library (funcutils)
  (export constantly vector-for-each-truthy for-each-indexed
		  dotimes curry
		  vector-find vector-index vector-popcnt -> ->> thunk)
  (import (chezscheme))

  (define (curry proc . curry-args)
	(lambda rest (apply proc (append curry-args rest))))

  ;; threading macros from clojure
  (define-syntax ->
	(syntax-rules ()
	  [(_ x) x]
	  [(_ x (f fargs ...) rst ...)
	   (-> (f x fargs ...) rst ...)]
	  [(_ x f rst ...)
	   (-> (f x) rst ...)]))

  (define-syntax ->>
	(syntax-rules ()
	  [(_ x) x]
	  [(_ x (f fargs ...) rst ...)
	   (->> (f fargs ... x) rst ...)]
	  [(_ x f rst ...)
	   (->> (f x) rst ...)]))

  (define-syntax thunk
	(syntax-rules ()
	  [(_ e1 e2 ...)
	   (lambda () e1 e2 ...)]))

  (define-syntax dotimes
	(syntax-rules ()
	  [(_ times e1 e2 ...)
	   (do [(i 0 (fx1+ i))]
		   [(fx= i times)]
		 e1 e2 ...)]))
  
  (define constantly
	;; some common stuff to avoid gc spamming
	(let* ([constantly-t (lambda _args #t)]
		   [constantly-f (lambda _args #f)])
	  (lambda (x)
		(cond
		 [(eq? #t x) constantly-t]
		 [(eq? #f x) constantly-f]
		 [else (lambda _args x)]))))

  (define (vector-for-each-truthy f v)
	(vector-for-each
	 (lambda (e) (when e (f e)))
	 v))

  (define (for-each-indexed f l)
	(do [(cur l (cdr cur))
		 (i 0 (fx1+ i))]
		[(null? cur)]
	  (f i (car cur))))

  (define (vector-index elem v)
	(define len (vector-length v))
	(let loop ([i 0])
	  (if (fx= i len)
		  #f
		  (let ([elemi (vector-ref v i)])
			(if (eq? elem elemi)
				i
				(loop (fx1+ i)))))))

  (define (vector-find proc v)
	(define len (vector-length v))
	(let loop ([i 0])
	  (if (fx= i len)
		  #f
		  (let ([elem (vector-ref v i)])
			(if (proc elem)
				elem
				(loop (fx1+ i)))))))

  (define (vector-popcnt v)
	(define len (vector-length v))
	(let loop ([i 0]
			   [n 0])
	  (if (fx>= i len)
		  n
		  (let ([elemi (vector-ref v i)])
			(if elemi
				(loop (fx1+ i) (fx1+ n))
				(loop (fx1+ i) n)))))))
