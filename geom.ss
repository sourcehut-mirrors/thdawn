;; Copyright (C) 2025 Vincent Lee; GPL-3.0-or-later
;; math and geometry library.
;; doesn't use the raylib ftypes, which would require manually managing memory
;; chez reader mode required to give names like -pi and -hpi
#!chezscheme
(library (geom)
  (export rectangle-x rectangle-y rectangle-width rectangle-height make-rectangle
		  v2x v2y vec2 v2+ v2- v2* v2zero v2unit v2sqrlen v2dot
		  check-collision-circles check-collision-recs check-collision-circle-rec
		  do-bounce-off
		  lerp eval-bezier-quad eval-bezier-cubic bezier-cubic-easing
		  pi -pi tau hpi -hpi
		  torad todeg eround epsilon-equal clamp
		  distsq
		  ease-in-quad ease-out-quad ease-in-out-quad
		  ease-out-cubic
		  ease-in-quart ease-out-quart ease-in-out-quart
		  ease-out-quint
		  ease-out-expo ease-out-circ)
  (import (chezscheme))

  (define (lerp a b progress)
	(+ a (* progress (- b a))))

  (define pi 3.141592653589793)
  (define -pi (fl- pi))
  (define hpi (fl/ pi 2.0))
  (define -hpi (fl- hpi))
  (define tau 6.283185307179586)
  (define (torad x)
	(fl* (fl/ pi 180.0) x))
  (define (todeg x)
	(fl* (fl/ 180.0 pi) x))
  (define (eround x)
	(exact (round x)))
  (define (epsilon-equal a b)
	(fl< (flabs (fl- a b)) 0.00000001))

  (define (clamp v lower upper)
	(max (min v upper) lower))
  (define (ease-out-cubic x)
	(- 1 (expt (- 1 x) 3.0)))
  (define (ease-in-out-quart x)
	(if (< x 0.5)
		(* 8 x x x x)
		(- 1 (/ (expt (+ (* -2 x) 2) 4)
				2))))
  (define (ease-in-quad x)
	(* x x))
  (define (ease-in-quart x)
	(* x x x x))
  (define (ease-out-quad x)
	(define invx (- 1 x))
	(- 1 (* invx invx)))
  (define (ease-in-out-quad x)
	(if (< x 0.5)
		(* 2 x x)
		(- 1 (/ (expt (+ (* -2 x) 2) 2)
				2))))
  (define (ease-out-quart x)
	(- 1 (expt (- 1 x) 4)))
  (define (ease-out-quint x)
	(- 1 (expt (- 1 x) 5)))
  (define (ease-out-expo x)
	(if (= 1 x)
		x
		(- 1 (expt 2 (* -10 x)))))
  (define (ease-in-circ x)
	(- 1 (sqrt (- 1 (* x x)))))
  (define (ease-out-circ x)
	(sqrt (- 1 (* (- x 1) (- x 1)))))
  (define (distsq x1 y1 x2 y2)
	(define dx (fl- x2 x1))
	(define dy (fl- y2 y1))
	(fl+ (fl* dx dx) (fl* dy dy)))

  (define-record-type (rectangle $make-rectangle rectangle?)
	(fields
	 x y width height)
	(sealed #t))
  (define (make-rectangle x y w h)
	;; will assert in dev, but be optimized away in prod
	($make-rectangle (fl+ x) (fl+ y) (fl+ w) (fl+ h)))

  (define-record-type vector2
	(fields x y)
	(sealed #t))
  (alias v2x vector2-x)
  (alias v2y vector2-y)

  (define (vec2 x y)
	;; will assert in dev, but be optimized away in prod
	(make-vector2 (fl+ x) (fl+ y)))

  (define v2zero (vec2 0.0 0.0))

  (define (v2unit v)
	(define x (v2x v))
	(define y (v2y v))
	(define norm (flsqrt (fl+ (fl* x x) (fl* y y))))
	(vec2 (fl/ x norm) (fl/ y norm)))

  (define (v2sqrlen v)
	(define x (v2x v))
	(define y (v2y v))
	(fl+ (fl* x x) (fl* y y)))

  (define (v2dot a b)
	(fl+ (fl* (v2x a) (v2x b))
		 (fl* (v2y a) (v2y b))))

  (define v2+
	(case-lambda
	  [(a b)
	   (vec2
		(fl+ (v2x a) (v2x b))
		(fl+ (v2y a) (v2y b)))]
	  [(a b . rest)
	   (vec2
		(fold-left (lambda (acc v) (fl+ acc (v2x v)))
				   (fl+ (v2x a) (v2x b))
				   rest)
		(fold-left (lambda (acc v) (fl+ acc (v2y v)))
				   (fl+ (v2y a) (v2y b))
				   rest))]))

  (define (v2- a b)
	(vec2
	 (fl- (v2x a) (v2x b))
	 (fl- (v2y a) (v2y b))))

  (define (v2* v scalar)
	(vec2
	 (fl* scalar (v2x v))
	 (fl* scalar (v2y v))))

  ;; NB: Collides when the two circles are orthogonal,
  ;; Not when they have any intersection.
  ;; This is what the original games use and allow for a fuzz factor of 2r_1r_2.
  ;; Refs https://mathworld.wolfram.com/OrthogonalCircles.html
  ;; https://thwiki.cc/%E6%B8%B8%E6%88%8F%E6%94%BB%E7%95%A5/STG%E5%88%A4%E5%AE%9A%E6%95%B0%E6%8D%AE
  (define (check-collision-circles x1 y1 radius1 x2 y2 radius2)
	(fl<= (distsq x1 y1 x2 y2)
		  (fl+ (fl* radius1 radius1) (fl* radius2 radius2))))

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
	(define p0x (v2x p0))
	(define p0y (v2y p0))
	(define p1x (v2x p1))
	(define p1y (v2y p1))
	(define p2x (v2x p2))
	(define p2y (v2y p2))

	(define x (lerp (lerp p0x p1x t)
					(lerp p1x p2x t)
					t))
	(define y (lerp (lerp p0y p1y t)
					(lerp p1y p2y t)
					t))
	(values x y))

  (define (eval-bezier-cubic p0 p1 p2 p3 t)
	(define invt (fl- 1.0 t))
	(v2+
	 (v2* p0 (* invt invt invt))
	 (v2* p1 (* 3 invt invt t))
	 (v2* p2 (* 3 invt t t))
	 (v2* p3 (* t t t))))

  ;; CSS-style cubic-bezier()
  ;; A cubic bezier curve takes 4 control points plus t and returns another point
  ;; A cubic bezier *easing function* fixes the first and last control points
  ;; at [0,0] and [1,1], treats x as the input between 0.0 and 1.0,
  ;; and treats y as the output.
  ;; Therefore, we need to turn the "t" input of the easing function (which is really
  ;; the bezier curve's x) into the bezier curve's t, then compute y from that t.
  ;; This requires some numerical methods detailed on the following page:
  ;; https://probablymarcus.com/blocks/2015/02/26/using-bezier-curves-as-easing-functions.html
  ;; Implemented with reference to the Blink codebase, license: BSD
  ;; https://chromium.googlesource.com/chromium/blink/+/master/Source/platform/animation/UnitBezier.h
  ;; Haven't spent the time to fully study and understand the code
  ;; as it has some optimizations and simplifications in the formulas
  ;; so some of it is blind translating (for now)
  (define (bezier-cubic-easing x1 y1 x2 y2)
	(define epsilon 1e-7)
	;; mostly confused how these terms are defined vs the standard bezier formulas?
	(define cx (fl* 3.0 x1))
	(define bx (fl- (fl* 3.0 (fl- x2 x1)) cx))
	(define ax (fl- 1.0 cx bx))
	(define cy (fl* 3.0 y1))
	(define by (fl- (fl* 3.0 (fl- y2 y1)) cy))
	(define ay (fl- 1.0 cy by))
	(define (curve-x t)
	  (define inner (fl+ (fl* ax t) bx))
	  (define next (fl+ (fl* inner t) cx))
	  (fl* next t))
	(define (curve-y t)
	  (define inner (fl+ (fl* ay t) by))
	  (define next (fl+ (fl* inner t) cy))
	  (fl* next t))
	(define (curve-xprime t)
	  (define inner (fl+ (fl* 3.0 ax t)
						 (fl* 2.0 bx)))
	  (fl+ (fl* inner t) cx))
	(define (x->t x)
	  ;; Returns newton's method approximation or #f if unable
	  (define (try-newton)
		(let loop ([i 0]
				   [guess x])
		  (let ([err (fl- (curve-x guess) x)])
			(if (fl< (flabs err) epsilon)
				guess
				(let ([deriv (curve-xprime guess)])
				  (if (or (fx= i 7) (fl< (flabs deriv) 1e-6))
					  #f
					  (loop (fx1+ i)
							(fl- guess (fl/ err deriv)))))))))
	  (define (binsearch)
		(let loop ([lower 0.0]
				   [upper 1.0]
				   [guess x])
		  (if (fl< lower upper)
			  (let ([guess-x (curve-x guess)])
				(if (fl< (flabs (fl- guess-x x)) epsilon)
					guess
					(let* ([too-low (fl> x guess-x)]
						   [next-lower (if too-low guess lower)]
						   [next-upper (if (not too-low) guess upper)])
					  (loop next-lower next-upper
							(fl+ (fl* 0.5 (fl- next-upper next-lower))
								 next-lower)))))
			  ;; bsearch failed, just return whatever the final guess was.
			  ;; this is only here as an ultra fallback, it's exceedingly rare we'd
			  ;; take so many iters to exhaust all the floating point precision
			  ;; such that next-lower starts being greater than next-upper
			  guess)))
	  (or (try-newton) (binsearch)))
	(lambda (x) ;; "t" of the easing function, bezier curve's x
	  (unless (fl<= 0.0 x 1.0)
		(assert-unreachable))
	  (curve-y (x->t x))))

  ;; Next two functions derived from
  ;; https://github.com/raylib-extras/examples-c/blob/main/rect_circle_collisions/rect_circle_collisions.c
  ;; Copyright (c) 2022 Jeffery Myers, used under the Zlib license
  ;; Returns (hit point vec2, hit-horizontal-face bool)
  (define (point-nearest-rectangle p rect)
	(define px (v2x p))
	(define py (v2y p))
	(define rx (rectangle-x rect))
	(define ry (rectangle-y rect))
	(define rw (rectangle-width rect))
	(define rh (rectangle-height rect))

	;; collisions with vertical surfaces
	(define-values (hit-x xdir)
	  (if (fl> px (fl+ rx rw))
		  (values (fl+ rx rw) 1.0)
		  (values rx -1.0)))
	(define nearest-vert
	  (let ([dist-from-ry (fl- py ry)])
		(vec2
		 hit-x
		 (cond
		  [(fl< dist-from-ry 0.0) ry]
		  [(fl>= dist-from-ry rh) (fl+ ry rh)]
		  [else (fl+ ry dist-from-ry)]))))

	;; collisions with horizontal surfaces
	(define-values (hit-y ydir)
	  (if (fl> py (fl+ ry rh))
		  (values (fl+ ry rh) 1.0)
		  (values ry -1.0)))
	(define nearest-horiz
	  (let ([dist-from-rx (fl- px rx)])
		(vec2
		 (cond
		  [(fl< dist-from-rx 0.0) rx]
		  [(fl> dist-from-rx rw) (fl+ rx rw)]
		  [else (fl+ rx dist-from-rx)])
		 hit-y)))
	(if (fl< (v2sqrlen (v2- p nearest-vert))
			 (v2sqrlen (v2- p nearest-horiz)))
		(values nearest-vert #f)
		(values nearest-horiz #t)))

  ;; returns (new p, new facing)
  (define (do-bounce-off p r facing rects)
	(define result
	  (fold-left
	   (lambda (acc rect)
		 (define p (vector-ref acc 0))
		 (define-values (hitpoint hit-horizontal-face)
		   (point-nearest-rectangle p rect))
		 (define to-hit (v2- hitpoint p))
		 (when (fl< (v2sqrlen to-hit) (fl* r r))
		   ;; Compute deepest part of ball
		   (let* ([proj (v2+ p (v2* (v2unit to-hit) r))]
				  ;; Nudge enough to get deepest part out to the hitpoint
				  [new-p
				   (if hit-horizontal-face
					   (vec2 (v2x p)
							 (fl+ (v2y p) (fl- (v2y hitpoint) (v2y proj))))
					   (vec2 (fl+ (v2x p) (fl- (v2x hitpoint) (v2x proj)))
							 (v2y p)))])
			 (vector-set! acc 0 new-p))
		   ;; Bounce
		   (if hit-horizontal-face
			   (vector-set! acc 2 (fl- (vector-ref acc 2)))
			   (vector-set! acc 1 (fl- (vector-ref acc 1)))))
		 acc)
	   (vector p (flcos facing) (flsin facing))
	   rects))
	(values (vector-ref result 0)
			(flatan (vector-ref result 2)
					(vector-ref result 1))))

  )
