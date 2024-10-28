;; userland geometry library to avoid having to use the raylib ftypes, which
;; require manually managing memory
(library (geom)
  (export rectangle-x rectangle-y rectangle-width rectangle-height make-rectangle
		  (rename (make-vector2 vec2) (vector2-x v2x) (vector2-y v2y))
		  v2+ v2- v2* v2zero v2unit
		  check-collision-circles check-collision-recs check-collision-circle-rec)
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
	(define norm (flsqrt (fl+ (fl* x x) (fl* y y))))
	(make-vector2 (fl/ x norm) (fl/ y norm)))

  (define (v2+ a b)
	(make-vector2
	 (fl+ (vector2-x a) (vector2-x b))
	 (fl+ (vector2-y a) (vector2-y b))))

  (define (v2- a b)
	(make-vector2
	 (fl- (vector2-x a) (vector2-x b))
	 (fl- (vector2-y a) (vector2-y b))))

  (define (v2* v scalar)
	(make-vector2
	 (fl* scalar (vector2-x v))
	 (fl* scalar (vector2-y v))))

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
  )
