;; userland geometry library to avoid having to use the raylib ftypes, which
;; require manually managing memory
(library (geom)
  (export rectangle-x rectangle-y rectangle-width rectangle-height make-rectangle
		  v2+ v2* v2x v2y vec2 v2zero v2unit
		  check-collision-circles check-collision-recs)
  (import (chezscheme))

  (define-record-type rectangle
	(fields
	 x y width height))

  (define-record-type vector2
	(fields x y))
  (define vec2 make-vector2)
  (define v2x vector2-x)
  (define v2y vector2-y)
  (define v2zero (vec2 0.0 0.0))

  (define (v2unit v)
	(define x (v2x v))
	(define y (v2y v))
	(define norm (sqrt (+ (* x x) (* y y))))
	(vec2 (/ x norm) (/ y norm)))

  (define (v2+ a b)
	(make-vector2
	 (+ (v2x a) (v2x b))
	 (+ (v2y a) (v2y b))))

  (define (v2* v scalar)
	(make-vector2
	 (* scalar (v2x v))
	 (* scalar (v2y v))))

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
