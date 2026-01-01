;; Copyright (C) 2025 Vincent Lee; GPL-3.0-or-later
(library (funcutils)
  (export constantly vector-for-each-truthy for-each-indexed
		  dotimes curry vector-for-all vector-for-each-indexed
		  vector-find vector-find-index vector-index vector-popcnt -> ->> thunk
		  vector-add vector-pop vector-truncate when-let assqdr)
  (import (chezscheme))

  (define (assqdr key alist)
	(cdr (assq key alist)))

  (define (curry proc . curry-args)
	(lambda rest (apply proc (append curry-args rest))))

  (define (vector-for-all proc v)
	(define n (vector-length v))
	(let loop ([i 0])
	  (or (fx= i n)
		  (and (proc (vector-ref v i))
			   (loop (fx1+ i))))))

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

  (define-syntax when-let
	(syntax-rules ()
	  [(_ ([var expr]) b1 b2 ...)
	   (let ([var expr])
		 (when var
		   b1 b2 ...))]))
  
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

  (define (vector-for-each-indexed f v)
	(define n (vector-length v))
	(do [(i 0 (fx1+ i))]
		[(fx= i n)]
	  (f i (vector-ref v i))))

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

  (define (vector-find-index proc v)
	(define len (vector-length v))
	(let loop ([i 0])
	  (if (fx= i len)
		  #f
		  (let ([elem (vector-ref v i)])
			(if (proc elem)
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
				(loop (fx1+ i) n))))))

  (define (vector-add v elem)
	(define length (vector-length v))
	(define result (make-vector (add1 length)))
	(vector-copy! v 0 result 0 length)
	(vector-set! result length elem)
	result)

  (define vector-pop
	(case-lambda
	  [(v) (vector-pop v 1)]
	  [(v n)
	   (define length (vector-length v))
	   (define result (make-vector (- length n)))
	   (vector-copy! v 0 result 0 (- length n))
	   result]))

  (define (vector-truncate v new-length)
	(define length (vector-length v))
	(define result (make-vector new-length))
	(vector-copy! v 0 result 0 new-length)
	result)
  )
