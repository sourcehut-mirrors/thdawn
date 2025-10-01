;; Copyright (C) 2025 Vincent Lee; GPL-3.0-or-later
;; userland geometry library to avoid having to use the raylib ftypes, which
;; require manually managing memory
(library (geom)
  (export rectangle-x rectangle-y rectangle-width rectangle-height make-rectangle
		  (rename (vector2-x v2x) (vector2-y v2y))
		  vec2 v2+ v2- v2* v2zero v2unit
		  check-collision-circles check-collision-recs check-collision-circle-rec
		  lerp eval-bezier-quad eval-bezier-cubic)
  (import (chezscheme))

  (define (lerp a b progress)
	(+ a (* progress (- b a))))

  (define-record-type rectangle
	(fields
	 x y width height))

  (define-record-type vector2
	(fields x y))

  (define (vec2 x y)
	;; will assert in dev, but be optimized away in prod
	(make-vector2 (fl+ x) (fl+ y)))
  
  (define v2zero (vec2 0.0 0.0))

  (define (v2unit v)
	(define x (vector2-x v))
	(define y (vector2-y v))
	(define norm (flsqrt (fl+ (fl* x x) (fl* y y))))
	(vec2 (fl/ x norm) (fl/ y norm)))

  (define v2+
	(case-lambda
	  [(a b)
	   (vec2
		(fl+ (vector2-x a) (vector2-x b))
		(fl+ (vector2-y a) (vector2-y b)))]
	  [(a b . rest)
	   (vec2
		(fold-left (lambda (acc v) (fl+ acc (vector2-x v)))
				   (fl+ (vector2-x a) (vector2-x b))
				   rest)
		(fold-left (lambda (acc v) (fl+ acc (vector2-y v)))
				   (fl+ (vector2-y a) (vector2-y b))
				   rest))]))

  (define (v2- a b)
	(vec2
	 (fl- (vector2-x a) (vector2-x b))
	 (fl- (vector2-y a) (vector2-y b))))

  (define (v2* v scalar)
	(vec2
	 (fl* scalar (vector2-x v))
	 (fl* scalar (vector2-y v))))

  ;; NB: Collides when the two circles are orthogonal,
  ;; Not when they have any intersection.
  ;; This is what the original games use and allow for a fuzz factor of 2r_1r_2.
  ;; Refs https://mathworld.wolfram.com/OrthogonalCircles.html
  ;; https://thwiki.cc/%E6%B8%B8%E6%88%8F%E6%94%BB%E7%95%A5/STG%E5%88%A4%E5%AE%9A%E6%95%B0%E6%8D%AE
  (define (check-collision-circles x1 y1 radius1 x2 y2 radius2)
	(define dx (fl- x2 x1))
	(define dy (fl- y2 y1))
	(define dist-sq (fl+ (fl* dx dx) (fl* dy dy)))
	(fl<= dist-sq (fl+ (fl* radius1 radius1) (fl* radius2 radius2))))

  (define (check-collision-recs x1 y1 w1 h1 x2 y2 w2 h2)
	(and (< x1 (+ x2 w2))
		 (> (+ x1 w1) x2)
		 (< y1 (+ y2 h2))
		 (> (+ y1 h1) y2)))

  (define (check-collision-circle-rec
		   x1 y1 r
		   x2 y2 w h)
	(define half-w (/ w 2))
	(define half-h (/ h 2))
	(define rec-center-x (+ x2 half-w))
	(define rec-center-y (+ y2 half-h))
	(define dx (abs (- x1 rec-center-x)))
	(define dy (abs (- y1 rec-center-y)))
	(cond
	 [(or (> dx (+ half-w r))
		  (> dy (+ half-h r)))
	  #f]
	 [(or (<= dx half-w)
		  (<= dy half-h))
	  #t]
	 [else
	  (let ([corner-distance-sq
			 (+ (* (- dx half-w) (- dx half-w))
				(* (- dy half-h) (- dy half-h)))])
		(<= corner-distance-sq (* r r)))]))

  (define (eval-bezier-quad p0 p1 p2 t)
	(define p0x (vector2-x p0))
	(define p0y (vector2-y p0))
	(define p1x (vector2-x p1))
	(define p1y (vector2-y p1))
	(define p2x (vector2-x p2))
	(define p2y (vector2-y p2))
	
	(define x (lerp (lerp p0x p1x t)
					(lerp p1x p2x t)
					t))
	(define y (lerp (lerp p0y p1y t)
					(lerp p1y p2y t)
					t))
	(values x y))

  (define (eval-bezier-cubic p0 p1 p2 p3 t)
	(define invt (- 1 t))
	(v2+
	 (v2* p0 (* invt invt invt))
	 (v2* p1 (* 3 invt invt t))
	 (v2* p2 (* 3 invt t t))
	 (v2* p3 (* t t t))))

  )
