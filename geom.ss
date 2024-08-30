;; userland geometry library to avoid having to use the raylib ftypes, which
;; require manually managing memory
(library (geom)
  (export rectangle-x rectangle-y rectangle-width rectangle-height make-rectangle
		  (rename (make-vector2 vec2) (vector2-x v2x) (vector2-y v2y))
		  v2+ v2* v2zero v2unit
		  check-collision-circles check-collision-recs)
  (import (chezscheme))

  (define-record-type rectangle
	(fields
	 x y width height))

  (define-record-type vector2
	(fields x y))
  (define v2zero (make-vector2 0.0 0.0))

  (define (v2unit v)
	(define x (vector2-x v))
	(define y (vector2-y v))
	(define norm (sqrt (+ (* x x) (* y y))))
	(make-vector2 (/ x norm) (/ y norm)))

  (define (v2+ a b)
	(make-vector2
	 (+ (vector2-x a) (vector2-x b))
	 (+ (vector2-y a) (vector2-y b))))

  (define (v2* v scalar)
	(make-vector2
	 (* scalar (vector2-x v))
	 (* scalar (vector2-y v))))

  (define (check-collision-circles x1 y1 radius1 x2 y2 radius2)
	(define dx (fl- x2 x1))
	(define dy (fl- y2 y1))
	(define dist-sq (fl+ (fl* dx dx) (fl* dy dy)))
	(define radius-sum (fl+ radius1 radius2))
	(fl<= dist-sq (fl* radius-sum radius-sum)))

  (define (check-collision-recs x1 y1 w1 h1 x2 y2 w2 h2)
	(and (< x1 (+ x2 w2))
		 (> (+ x1 w1) x2)
		 (< y1 (+ y2 h2))
		 (> (+ y1 h1) y2)))

  )
