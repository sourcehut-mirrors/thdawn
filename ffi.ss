
(define-ftype Color
  (struct (r unsigned-8) (g unsigned-8) (b unsigned-8) (a unsigned-8)))
(printf "~a" (ftype-sizeof Color))
(define ptr (make-ftype-pointer Color (foreign-alloc (ftype-sizeof Color))))
(printf "~a" (ftype-pointer->sexpr ptr))

  (define-ftype Color
	(struct (r unsigned-8) (g unsigned-8) (b unsigned-8) (a unsigned-8)))

  (define (make-color r g b a)
	(define ret (make-ftype-pointer Color (foreign-alloc (ftype-sizeof Color))))
	(ftype-set! Color (r) ret r)
	(ftype-set! Color (g) ret g)
	(ftype-set! Color (b) ret b)
	(ftype-set! Color (a) ret a)
	ret)

(printf "~a" (ftype-pointer->sexpr (make-color 1 2 3 300)))
