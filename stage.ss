(define (ch0-w12-fairy bullet xvel task enm)
  (define (movement task)
	(loop-until
	 (> (enm-y enm) 200.0)
	 (enm-y-set! enm (+ (enm-y enm) 2.2)))
	(loop-until
	 (> (enm-y enm) (+ +playfield-max-y+ 20))
	 (enm-y-set! enm (+ (enm-y enm) 1.7))
	 (enm-x-set! enm (+ (enm-x enm) xvel)))
	(delete-enemy enm))
  (define (shoot task)
	(wait 30)
	(dotimes 4
	  (-> (fb)
		  (fbcounts 1 3)
		  (fbspeed 4.0 6.0)
		  (fbshootenm enm bullet 5 (sebundle-shoot0 sounds)))
	  (wait 50)))
  (spawn-subtask "movement" movement (constantly #t) task)
  (spawn-subtask "shoot" shoot (constantly #t) task)
  (wait-until (constantly #f)))

(define (ch0-big-fairy task enm)
  (define start-time frames)
  (define (movement task)
	(loop-until
	 (> (enm-y enm) 140.0)
	 (enm-y-set! enm (+ (enm-y enm) 1.2)))
	(wait 300)
	(ease-to values 250.0 160.0 180 enm)
	(delete-enemy enm))
  (define (rain task)
	(define pat
	  (-> (fb)
		  (fbcounts 10 1)
		  (fbspeed 4.0 6.0)
		  (fbabsolute-aim)
		  (fbang 270.0 6.0)))
	(define types '#(small-ball-red small-ball-orange small-ball-blue
									small-ball-magenta small-ball-yellow
									small-ball-white))
	(let loop ([i 0])
	  (fbshoot pat
			   (enm-x enm) (enm-y enm)
			   (lambda (row col speed facing)
				 (spawn-bullet (vnth types i) (enm-x enm) (enm-y enm) 5
							   (curry linear-step-gravity-forever
									  (fl+ facing
										   (centered-roll game-rng (torad 10.0)))
									  speed 0.1))))
	  (wait 20)
	  (when (< (- frames start-time) 200)
		(loop (mod (add1 i) (vlen types))))))
  (define (ring task)
	(interval-loop 50
	  (-> (cb)
		  (cbcount 15)
		  (cbang 15.0 0.0)
		  (cbspeed 3.0 3.0)
		  (cbshootenm enm 'music-blue 5 (sebundle-bell sounds)))))
  (define (note task)
	(wait 200)
	(for-each
	 (lambda (xoff yoff)
	   (dotimes 10
		 (spawn-bullet 'pellet-blue (- (enm-x enm) 40.0) (+ yoff (enm-y enm)) 5
					   (lambda (blt)
						 (wait-until (thunk (>= (- frames start-time) 350)))
						 (linear-step-forever (* tau (roll game-rng)) 2.0 blt))))
	   (unless (flzero? xoff)
		 (dotimes 10
		   (spawn-bullet 'pellet-blue (+ xoff (enm-x enm) -40.0)
						 (+ yoff (enm-y enm)) 5
					   (lambda (blt)
						 (wait-until (thunk (>= (- frames start-time) 350)))
						 (linear-step-forever (* tau (roll game-rng)) 2.0 blt)))))
	   (wait 3))
	 '(5.0 10.0 12.0 15.0 15.0 17.0 17.0 12.0 11.0 0.0 0.0 0.0 0.0 0.0)
	 '(-40.0 -35.0 -30.0 -25.0 -20.0 -15.0 -10.0 -5.0 0.0 5.0 10.0 15.0 20.0
			 25.0))
	(spawn-bullet 'big-star-blue (- (enm-x enm) 50.0) (+ 28.0 (enm-y enm))
				  5 (lambda (blt)
					  (wait-until (thunk (>= (- frames start-time) 350)))
					  (raylib:play-sound (sebundle-bell sounds))
					  (linear-step-forever (* tau (roll game-rng)) 2.0 blt))))
  (spawn-subtask "movement" movement (constantly #t) task)
  (spawn-subtask "rain" rain (constantly #t) task)
  (spawn-subtask "ring" ring (thunk (fx< (fx- frames start-time) 250)) task)
  (spawn-subtask "note" note (constantly #t) task)
  (wait-until (constantly #f)))

(define (ch0-w3-fairy type task enm)
  (define x0 (enm-x enm))
  (define y0 (enm-y enm))
  (ease-linear-to (+ x0 40.0) y0 10 enm)
  (let ([l (spawn-laser type (+ (enm-x enm) 20.0) (enm-y enm)
			   0.0 (inexact +playfield-width+)
			   5.0
			   40 20 (lambda (_blt) (wait-until (thunk (>= frames 900)))))])
	(bullet-addflags l (bltflags uncancelable)))
  (raylib:play-sound (sebundle-laser sounds))
  (wait-until (thunk (>= frames 880)))
  (ease-linear-to x0 y0 20 enm)
  (delete-enemy enm))

(define (chapter0 task)
  (set! current-chapter 0)
  (wait 120)
  ;; wave1
  (let ([w1 (curry ch0-w12-fairy 'music-red 1.95)])
	(spawn-enemy (enmtype red-fairy) -150.0 -130.0 80 w1 )
	(spawn-enemy (enmtype red-fairy) -150.0 -100.0 80 w1)
	(spawn-enemy (enmtype red-fairy) -150.0 -70.0 80 w1)
	(spawn-enemy (enmtype red-fairy) -150.0 -40.0 80 w1)
	(spawn-enemy (enmtype red-fairy) -150.0 -10.0 80 w1))
  
  ;; wave 2
  (wait 200)
  (let ([w2 (curry ch0-w12-fairy 'music-orange -1.95)])
	(spawn-enemy (enmtype yellow-fairy) 150.0 -130.0 80 w2)
	(spawn-enemy (enmtype yellow-fairy) 150.0 -100.0 80 w2)
	(spawn-enemy (enmtype yellow-fairy) 150.0 -70.0 80 w2)
	(spawn-enemy (enmtype yellow-fairy) 150.0 -40.0 80 w2)
	(spawn-enemy (enmtype yellow-fairy) 150.0 -10.0 80 w2))

  (wait 200)
  (spawn-enemy (enmtype big-fairy) 0.0 -10.0 3000 ch0-big-fairy
			   `((point . 20)))

  (wait 190)
  (spawn-enemy (enmtype red-fairy) -220.0 115.0 50
			   (curry ch0-w3-fairy 'fixed-laser-red))
  (wait 25)
  (spawn-enemy (enmtype green-fairy) -220.0 130.0 50
			   (curry ch0-w3-fairy 'fixed-laser-orange))
  (wait 25)
  (spawn-enemy (enmtype blue-fairy) -220.0 145.0 50
			   (curry ch0-w3-fairy 'fixed-laser-blue))
  (wait 25)
  (spawn-enemy (enmtype yellow-fairy) -220.0 160.0 50
			   (curry ch0-w3-fairy 'fixed-laser-magenta))
  (wait 25)
  (spawn-enemy (enmtype red-fairy) -220.0 175.0 50
			   (curry ch0-w3-fairy 'fixed-laser-yellow))
  (wait-until (thunk (>= frames 870)))
  (chapter1 task))

(define (ch1-big-fairy flip task enm)
  (define (shoot task)
	(wait 10)
	(do [(i 0 (fx1+ i))]
		[(fx= i 35)]
	  (let ([facing (torad (fl* (fl/ 360.0 20.0) (inexact i)))]
			[cfun (lambda (blt)
					(linear-step-forever (facing-player (bullet-x blt) (bullet-y blt))
										 7.0 blt))])
		(-> (spawn-bullet 'ellipse-magenta
						  (fl+ (enm-x enm) (fl* 40.0 (flcos facing)))
						  (fl+ (enm-y enm) (fl* 40.0 (flsin facing)))
						  10 cfun)
			(bullet-facing-set! facing))
		(-> (spawn-bullet 'ellipse-magenta
						  (fl+ (enm-x enm) (fl* 40.0 (flcos (fl+ facing pi))))
						  (fl+ (enm-y enm) (fl* 40.0 (flsin (fl+ facing pi))))
						  10 cfun)
			(bullet-facing-set! facing)))
	  (wait 3)))
  (define points (vector (vec2 -141.0 0.0)
						 (vec2 -151.0 285.0)
						 (vec2 134.0 276.0)
						 (vec2 220.0 214.0)))
  (spawn-subtask "shoot" shoot (constantly #t) task)
  (move-on-spline (if flip (vector-map (lambda (p) (vec2 (fl- (v2x p)) (v2y p)))
									   points)
					  points)
				  (lambda (_) (values values 180))
				  enm)
  (delete-enemy enm))

(define (ch1-small-fairy task enm)
  (define (shoot task)
	(interval-loop 20
	  (-> (fb)
		  (fbcounts 4 3)
		  (fbang 0.0 25.0)
		  (fbspeed 6.0 7.0)
		  (fbshootenm enm 'small-ball-red 5 (sebundle-shoot0 sounds)))))
  (define facing (facing-player (enm-x enm) (enm-y enm)))
  (define (move task)
	;; FIXME: this breaks at shallow angles
	(interval-loop-while 1 (< (enm-y enm) 470.0)
	  (linear-step-enm facing 5.0 enm)))
  (spawn-subtask "shoot" shoot (thunk (fl< (enm-y enm) 350.0)) task)
  (move task)
  (delete-enemy enm))

(define (chapter1 task)
  (set! current-chapter 1)
  (wait 50)
  (spawn-enemy (enmtype big-fairy) -141.0 0.0 600 (curry ch1-big-fairy #f)
			   five-point-items)
  (wait 225)
  (dotimes 10
	(spawn-enemy (enmtype red-fairy) -141.0 0.0 50 ch1-small-fairy)
	(wait 7))
  (wait 70)
  (spawn-enemy (enmtype big-fairy) 141.0 0.0 600 (curry ch1-big-fairy #t)
			   ten-point)
  (wait 220)
  (dotimes 10
	(spawn-enemy (enmtype red-fairy) 141.0 0.0 50 ch1-small-fairy)
	(wait 7))
  (wait-until (thunk (>= frames 1645)))
  (chapter2 task))

(define (ch2-w1-fairy flip task enm)
  (define start-time frames)
  (define (shoot task)
	(wait 30)
	(interval-loop 80
	  (-> (cb)
		  (cbcount 50 3)
		  (cbang 0.0 (if flip 5.0 -5.0))
		  (cbspeed 3.0 4.0)
		  (cbshootenm enm 'small-star-green 5 (sebundle-bell sounds)))))
  (spawn-subtask "shoot" shoot (thunk (fx< (fx- frames start-time) 240)) task)
  (ease-to values (if flip 160.0 -160.0) 150.0 30 enm)
  (wait 240)
  (move-on-spline
   (if flip
	   (vector (vec2 (enm-x enm) (enm-y enm))
			   (vec2 47.0 85.0)
			   (vec2 -51.0 149.0)
			   (vec2 -89.0 -20.0))
	   (vector (vec2 (enm-x enm) (enm-y enm))
			   (vec2 -47.0 85.0)
			   (vec2 51.0 149.0)
			   (vec2 89.0 -20.0)))
   (lambda (_) (values values 180))
   enm)
  (delete-enemy enm))
(define (ch2-w2-fairy task enm)
  (ease-to values (enm-x enm) 80.0 30 enm)
  (-> (cb)
	  (cbcount 36 4)
	  (cbspeed 5.0 6.0)
	  (cbshootenm enm 'big-star-magenta 5 (sebundle-shoot0 sounds)))
  (wait 180)
  (ease-to values (enm-x enm) -20.0 60 enm)
  (delete-enemy enm))
(define (ch2-w3-fairy task enm)
  (define x (enm-x enm))
  (define pat
	(-> (cb)
		(cbcount 35 1)
		(cbspeed 5.0)
		;; TODO i don't really understand this value? why does it have to be so big?
		(cbang 140.0 0.0)))
  (ease-to values (if (negative? x) (+ x 80.0) (- x 80.0)) (enm-y enm) 60 enm)
  (wait 80)
  (cbshootenm pat enm 'rest-red 5 (sebundle-shoot0 sounds))
  (wait 25)
  (cbshootenm pat enm 'rest-orange 5 (sebundle-shoot0 sounds))
  (wait 12)
  (cbshootenm pat enm 'rest-blue 5 (sebundle-shoot0 sounds))
  (wait 12)
  (cbshootenm pat enm 'rest-magenta 5 (sebundle-shoot0 sounds))
  (ease-to values x (enm-y enm) 60 enm)
  (delete-enemy enm))
(define (chapter2 task)
  (set! current-chapter 2)
  (spawn-enemy (enmtype medium-blue-fairy) 220.0 150.0 500 (curry ch2-w1-fairy #t)
			   five-point-items)
  (wait 220)
  (spawn-enemy (enmtype medium-blue-fairy) -220.0 150.0 500 (curry ch2-w1-fairy #f)
			   five-point-items)
  (wait 180)
  (spawn-enemy (enmtype medium-red-fairy) -100.0 -20.0 350 ch2-w2-fairy
			   five-point-items)
  (wait 30)
  (spawn-enemy (enmtype medium-red-fairy) 0.0 -20.0 350 ch2-w2-fairy
			   five-point-items)
  (wait 30)
  (spawn-enemy (enmtype medium-red-fairy) 100.0 -20.0 350 ch2-w2-fairy
			   five-point-items)
  (wait 120)
  (spawn-enemy (enmtype big-fairy) -220.0 180.0 500 ch2-w3-fairy ten-point)
  (spawn-enemy (enmtype big-fairy) 220.0 180.0 500 ch2-w3-fairy ten-point)
  (wait-until (thunk (>= frames 2425)))
  (chapter3 task))

(define (ch3-fairy-sin-move flip task enm)
  (define start-x (enm-x enm))
  (define start-y (enm-y enm))
  (do [(i 0 (add1 i))]
	  [(if flip
		   (<= (enm-x enm) (- +playfield-min-x+ 50))
		   (>= (enm-x enm) (+ +playfield-max-x+ 50)))]
	(let ([x ((if flip fl- fl+) start-x (fl* (inexact i) 1.7))]
		  [y (fl+ start-y (fl* 20.0 (flsin (fl/ (inexact i) 18.0))))])
	  (enm-x-set! enm x)
	  (enm-y-set! enm y))
	(yield))
  (delete-enemy enm))
(define (ch3-w1-leader-fairy flip task enm)
  (define (shoot task)
	(interval-loop 60
	  (-> (cb)
		  (cbcount 12)
		  (cbspeed 4.0)
		  (cbshootenm enm 'medium-ball-cyan 5 (sebundle-bell sounds)))))
  (enm-addflags enm (enmflags invincible))
  (spawn-subtask "uninvincible"
	(lambda (task)
	  (wait 90)
	  (enm-clrflags enm (enmflags invincible)))
	(constantly #t)
	task)
  (spawn-subtask "shoot" shoot (constantly #t) task)
  (ch3-fairy-sin-move flip task enm))
(define (ch3-w1-leader-on-death followers)
  (for-each
   (lambda (f)
	 (when (> (enm-health f) 0)
	   (enm-drops-set! f default-drop)
	   (kill-enemy f)
	   (-> (cb)
		   (cbcount 12 2)
		   (cbspeed 2.0 3.0)
		   (cbshootenm f 'small-ball-white 2
					  (sebundle-bell sounds)))))
   followers))

(define (ch3-w1-follower-fairy delay flip task enm)
  (define (shoot task)
	(interval-loop 30
	  (dotimes 3
		(-> (fb)
			(fbabsolute-aim)
			(fbcounts 3)
			(fbspeed 4.0 4.0)
			(fbang (if flip 265.0 275.0) 10.0)
			(fbshoot (enm-x enm) (enm-y enm)
					 (lambda (row col speed facing)
					   (spawn-bullet
						(if flip 'small-ball-yellow 'small-ball-red)
						(enm-x enm) (enm-y enm) 5
						(lambda (blt)
						  (linear-step-gravity-forever facing speed 0.1 blt))))))
		(wait 2))))
  (spawn-subtask "uninvincible"
	(lambda (task)
	  (wait (+ delay 100))
	  (enm-clrflags enm (enmflags invincible)))
	(constantly #t)
	task)
  (enm-addflags enm (enmflags invincible))
  (wait delay)
  (spawn-subtask "shoot" shoot (constantly #t) task)
  (ch3-fairy-sin-move flip task enm))

(define (ch3-w2-fairy delay init-ang init-dist cx cy task enm)
  (define stop-spinning (box #f))
  (define sin72 (flsin (torad 72.0)))
  (define laser-dir (+ (- init-ang pi) (torad 18.0)))
  (define laser (spawn-laser 'fixed-laser-orange (enm-x enm) (enm-y enm) laser-dir
							 (fl* 2.0 init-dist sin72) 5.0 40 delay
							 (lambda (blt) (loop-until (unbox stop-spinning)))))
  (define (do-spin)
	(do [(angvel (torad 0.05) (if (fl< angvel (torad 4.0))
								  (fl+ angvel (torad 0.1))
								  angvel))
		 (ang init-ang (+ ang angvel))
		 (i 0 (add1 i))]
		[(= i 150) ang]
	  (let ([dist (lerp init-dist 80.0 (ease-out-quad (/ i 200)))])
		(enm-x-set! enm (+ cx (* dist (cos ang))))
		(enm-y-set! enm (+ cy (* dist (sin ang))))
		(bullet-x-set! laser (enm-x enm))
		(bullet-y-set! laser (enm-y enm))
		(bullet-facing-set! laser (+ (- ang pi) (torad 18.0)))
		;; The angle formed by the two points and the center is 144 degrees
		;; bisecting and using trig gives us this result
		(laser-length-set! laser (fl* 2.0 dist sin72)))
	  (yield)))
  (bullet-addflags laser (bltflags uncancelable))
  (raylib:play-sound (sebundle-laser sounds))
  (wait delay)
  (spawn-subtask "spin shoot"
	(lambda (task)
	  (interval-loop 2
		(let ([ang (todeg (atan (- (enm-y enm) cy) (- (enm-x enm) cx)))])
		  (-> (fb)
			  (fbcounts 1)
			  (fbspeed 9.0)
			  (fbang (- ang 90.0) 5.0)
			  (fbabsolute-aim)
			  (fbshootenm enm 'music-blue 5 #f)))))
	(thunk (not (unbox stop-spinning)))
	task)
  (let ([final-ang (do-spin)])
	(set-box! stop-spinning #t)
	(spawn-subtask "exit shoot"
				   (lambda (task)
					 (loop-forever
					   (-> (fb)
						   (fbcounts 2)
						   (fbspeed 6.0)
						   (fbang 0.0 12.0)
						   (fbshootenm enm 'small-star-red 2 #f))))
				   (constantly #t)
				   task)
	(ease-to values
			 (+ cx (* 300.0 (cos final-ang))) (+ cy (* 300.0 (sin final-ang)))
			 40 enm)
	(delete-enemy enm)))

(define (chapter3 task)
  (set! current-chapter 3)
  (wait 20)

  ;; ;; technically map can and does evaluate the lambda out of order, but here it
  ;; ;; doesn't really matter as long as it remains the same impl in chez scheme,
  ;; ;; which is likely the case.
  (let ([followers (map (lambda (i)
						  (spawn-enemy
						   (enmtype red-fairy) -210.0 150.0 100
						   (curry ch3-w1-follower-fairy (* 20 (add1 i)) #f)
						   five-point-items))
						(iota 6))])
	(spawn-enemy (enmtype big-fairy) -210.0 150.0 500
				 (curry ch3-w1-leader-fairy #f) five-point-items
				 (lambda () (ch3-w1-leader-on-death followers))))
  (wait 400)
  (let ([followers (map (lambda (i)
						  (spawn-enemy
						   (enmtype red-fairy) 220.0 180.0 100
						   (curry ch3-w1-follower-fairy (* 20 (add1 i)) #t)
						   five-point-items))
						(iota 6))])
	(spawn-enemy (enmtype big-fairy) 220.0 180.0 500
				 (curry ch3-w1-leader-fairy #t) five-point-items
				 (lambda () (ch3-w1-leader-on-death followers))))
  (wait 240)
  (dotimes 4
	(spawn-particle
	 (make-particle
	  (particletype circle-hint)
	  0.0 224.0
	  30 0 '((color . -1) (r1 . 300.0) (r2 . 80.0))))
	(wait 30))
  (wait 40)
  ;; TODO: hint particle telling player to get in middle of screen?
  (let ([cx 0.0]
		[cy 224.0]
		[delay-per 27]
		[initial-dist 225.0])
	(do [(i 0 (add1 i))
		 (delay (* 4 delay-per) (- delay delay-per))]
		[(= i 5)]
	  (let ([ang (torad (- (* 72.0 (inexact i)) 18.0))])
		(spawn-enemy (enmtype red-fairy)
					 (+ cx (* initial-dist (cos ang)))
					 (+ cy (* initial-dist (sin ang)))
					 50000 (curry ch3-w2-fairy delay ang initial-dist cx cy)))
	  (wait delay-per)))
  (wait-until (thunk (>= frames 3500)))
  (chapter4 task))

(define (boss-standard-wander-once enm duration)
  (define (pick-next-y)
	(clamp (fl+ (enm-y enm)
				(centered-roll game-rng 20.0))
		   80.0 140.0))
  (define (pick-next-x)
	(define ox (enm-x enm))
	(define mag (inexact (+ 15 (roll game-rng 65))))
	(cond
	 [(fl< ox -160.0)
	  (fl+ ox mag)]
	 [(fl< ox -80.0)
	  (if (fl< (roll game-rng) 0.3333)
		  (fl- ox mag)
		  (fl+ ox mag))]
	 [(fl< ox 80.0)
	  (if (fl< (roll game-rng) 0.5)
		  (fl- ox mag)
		  (fl+ ox mag))]
	 [(fl< ox 160.0)
	  (if (fl< (roll game-rng) 0.3333)
		  (fl+ ox mag)
		  (fl- ox mag))]
	 [else (fl- ox mag)]))
  (ease-to ease-in-out-quad (pick-next-x) (pick-next-y) duration enm))

(define (boss-standard-wander enm task)
  (interval-loop 80
	(boss-standard-wander-once enm 70)))

(define (position-bullets-around cx cy dist start-angle bullets)
  (define dang (fl/ tau (inexact (length bullets))))
  (let loop ([ang start-angle]
			 [bullets bullets])
	(unless (null? bullets)
	  (bullet-x-set! (car bullets) (fl+ cx (fl* dist (flcos ang))))
	  (bullet-y-set! (car bullets) (fl+ cy (fl* dist (flsin ang))))
	  (loop (fl+ ang dang)
			(cdr bullets)))))

(define (midboss-control task enm)
  (define bossinfo (enm-extras enm))
  (define (ring)
	(-> (cb)
		(cbcount 18 3)
		(cbspeed 5.0 6.0)
		(cbang 0.0 10.0)
		(cbshootenm enm 'heart-red 5 (sebundle-bell sounds))))
  (define (orb point with-ring)
	(define speed (list-ref point 2))
	(define facing (list-ref point 3))
	(define x (enm-x enm))
	(define y (enm-y enm))
	(define start-frames frames)
	(letrec ([center-blt
			  (spawn-bullet
			   'small-ball-red x y 5
			   (lambda (blt)
				 (loop-forever
				  (linear-step facing speed blt)
				  (when with-ring
					(position-bullets-around (bullet-x blt) (bullet-y blt)
											 33.0 0.0 ring))
				  (position-bullets-around (bullet-x blt) (bullet-y blt)
										   12.0 (torad (mod (* 2 (+ start-frames
																	frames))
															360.0))
										   notes))))]
			 [ring (if with-ring
					   (map
						(lambda (_)
						  (spawn-bullet
						   'pellet-white x y 5
						   (lambda (blt)
							 (wait-until
							  (thunk (not (vector-index center-blt live-bullets))))
							 (delete-bullet blt))))
						(iota 24))
					   '())]
			 [notes (map
					 (lambda (type)
					   (spawn-bullet
						type x y 5
						(lambda (blt)
						  (wait-until
						   (thunk (not (vector-index center-blt live-bullets))))
						  (delete-bullet blt))))
					 '(music-red music-yellow music-cyan))])
	  (void)))
  (ease-to values 0.0 80.0 20 enm)
  (raylib:play-sound (sebundle-longcharge sounds))
  (wait 40)

  (declare-spell enm 0)
  (enm-addflags enm (enmflags invincible))
  (cancel-all #f)
  (ease-to values 0.0 100.0 20 enm)
  (raylib:play-sound (sebundle-shortcharge sounds))
  (wait 60)

  (let loop ([first #t])
   (let ([points (-> (fb)
					 (fbcounts 8)
					 (fbang 0.0 25.0)
					 (fbspeed 6.0)
					 (fbcollect (enm-x enm) (enm-y enm)))])
	 (for-each
	  (lambda (point)
		(raylib:play-sound (sebundle-shoot0 sounds))
		(orb point #t) (wait 12))
	  points)
	 (ring) (wait 22)
	 (ring) (wait 22)
	 (ring)
	 (raylib:play-sound (sebundle-shortcharge sounds))
	 (boss-standard-wander-once enm 60)
	 (for-each
	  (lambda (point)
		(raylib:play-sound (sebundle-shoot0 sounds))
		(orb point #t) (wait 12))
	  (reverse points))
	 (ring) (wait 22)
	 (ring) (wait 22)
	 (ring)
	 (when first
	   (raylib:play-sound (sebundle-shortcharge sounds))
	   (boss-standard-wander-once enm 50)
	   (loop #f))))
  (wait 25)
  (raylib:play-sound (sebundle-shoot0 sounds))
  (cancel-all #f)
  (ease-to ease-in-out-quad 0.0 150.0 60 enm)
  (spawn-subtask "aim"
	(lambda (task)
	  (interval-loop 30 
		(-> (cb)
			(cbcount 14)
			(cbspeed 6.0)
			(cbshootenm enm 'ellipse-red 5 (sebundle-bell sounds)))
		))
	(thunk (positive? (bossinfo-remaining-timer bossinfo)))
	task)
  (let loop ([ang 0.0]
			 [offset 80.0])
	(for-each
	 (lambda (point) (orb point #f))
	 (-> (cb)
		 (cbcount 6)
		 (cbabsolute-aim)
		 (cbang ang)
		 (cbspeed 4.5)
		 (cboffset offset)
		 (cbcollect (enm-x enm) (enm-y enm))))
	(when (positive? (bossinfo-remaining-timer bossinfo))
	  (wait 7)
	  (loop (fl+ ang 35.0) (if (<= offset 38.0) offset (- offset 1.0)))))
  (common-spell-postlude bossinfo enm)
  (common-boss-postlude bossinfo enm #t)
  (wait 75)
  (ease-to values -100.0 -100.0 20 enm)
  (delete-enemy enm))

(define (chapter4 task)
  (set! current-chapter 4)
  (let ([enm (spawn-enemy (enmtype boss-doremi) 100.0 -100.0 500 midboss-control
						  '((bomb . 1) (point . 30))
						  (thunk #f))]
		[bossinfo (blank-bossinfo "Harukaze Doremi" #xff7fbcff)])
	(bossinfo-healthbars-set!
	 bossinfo
	 (immutable-vector (make-healthbar -1 0.0 #xf5f5f5ff #x808080ff)))
	(enm-extras-set! enm bossinfo))
  (wait-until (thunk (>= frames 5200)))
  (chapter5 task))

(define (ch5-bigfairy task enm)
  (ease-to values (enm-x enm) 100.0 60 enm)
  (spawn-subtask "rally"
	(lambda (task)
	  (define (shoot type ang spd)
		(define blt (spawn-bullet type
								  (+ (enm-x enm) -10.0 (* 5 (cos ang)))
								  (+ (enm-y enm) (* 5 (sin ang)))
								  2
								  (curry linear-step-gravity-forever ang spd
										 0.07 3.5)))
		(bullet-addflags blt (bltflags noprune))
		(spawn-task "prune" (lambda (task)
							  (wait 300)
							  (bullet-clrflags blt (bltflags noprune)))
					(constantly #t)))
	  (define angs
		(map torad '(210.0 230.0 250.0 270.0 290.0 310.0 330.0)))
	  (define types
		(map (lambda (color)
			   (string->symbol (string-append "droplet-" (symbol->string color))))
			 basic-colors))
	  (let loop ([angs angs]
				 [type-idx 0])
		(for-each
		 (lambda (ang)
		   (shoot (list-ref types type-idx)
				  (+ ang (centered-roll game-rng (torad 2.0))) 5.0)
		   (shoot (list-ref types type-idx)
				  (+ ang (centered-roll game-rng (torad 2.0))) 3.0)
		   (raylib:play-sound (sebundle-shoot0 sounds))
		   (wait 2))
		 (cdr angs))
		(loop (reverse! angs)
			  (mod (1+ type-idx) (length types)))))
	(constantly #t)
	task)
  (wait 300)
  (ease-to ease-in-quart 0.0 -50.0 180 enm)
  (delete-enemy enm))

(define (ch5-yinyang task enm)
  (spawn-subtask "shoot"
	(lambda (task)
	  (interval-loop 40
		(dotimes 5
		  (-> (fb)
			  (fbcounts 3)
			  (fbabsolute-aim)
			  (fbang -90.0 70.0)
			  (fbspeed 2.0)
			  (fbshootenm enm 'small-star-red 2 #f))
		  (-> (fb)
			  (fbcounts 3)
			  (fbabsolute-aim)
			  (fbang 90.0 70.0)
			  (fbspeed 4.0)
			  (fbshootenm enm 'small-star-orange 2 #f))
		  (wait 2))))
	(constantly #t)
	task)
  (loop-forever
   (linear-step-enm (torad 90.0) 2.0 enm)
   (when (> (enm-y enm) +playfield-max-y+)
	 (raylib:play-sound (sebundle-enmdie sounds))
	 (delete-enemy enm)
	 (-> (cb)
		 (cbcount 18)
		 (cbspeed 2.0)
		 (cbshootenm enm 'small-ball-magenta 5 (sebundle-bell sounds))))))

(define (chapter5 task)
  (define xs '((-105.0 . 20.0) (-20.0 . 40.0)
			   (-50.0 . 20.0) (-40.0 . 0.0)
			   (-90.0 . -20.0) (-10.0 . 10.0)
			   (-20.0 . 0.0) (0.0 . 20.0)
			   (-80.0 . -20.0) (-10.0 . 50.0)))
  (define yys-killed (box 0))
  (define (on-death)
	(define next (1+ (unbox yys-killed)))
	(set-box! yys-killed next)
	(when (= next (* 2 (length xs)))
	  ;; all yinyangs killed, reward
	  (spawn-drops '((bomb . 1) (life-frag . 1))
				   0.0 (inexact +poc-y+)))
	#t)
  (set! current-chapter 5)
  (spawn-enemy (enmtype big-fairy) 0.0 -20.0 2500 ch5-bigfairy
			   '((point . 20)))
  (wait 360)
  (for-each
   (lambda (pair)
	(spawn-enemy (enmtype magenta-yinyang)
				 (car pair) (centered-roll game-rng 10.0)
				 250 ch5-yinyang '((point . 5)) on-death)
	(spawn-enemy (enmtype magenta-yinyang)
				 (cdr pair) (centered-roll game-rng 10.0)
				 250 ch5-yinyang '((point . 5)) on-death)
	(wait 50))
   xs)
  (wait-until (thunk (>= frames 6339)))
  (chapter6 task))

(define (ch6-sswave x y even-type odd-type)
  (raylib:play-sound (sebundle-shoot0 sounds))
  (-> (cb)
	  (cbcount 20 3)
	  (cbang 0.0 10.0)
	  (cbspeed 2.0 3.5)
	  (cbshoot x y
		(lambda (layer in-layer speed facing)
		  (-> (spawn-bullet
			   (if (fxeven? layer) even-type odd-type)
			   x y 10
			   (lambda (blt)
				 (define initial-facing facing)
				 (define turn-dir (torad (if (fxeven? layer) -4.0 4.0)))
				 (let loop ([facing initial-facing])
				   (bullet-facing-set! blt facing)
				   (linear-step facing speed blt)
				   (yield)
				   (if (fl> (flabs (fl- facing initial-facing)) tau)
					   (begin
						 (raylib:play-sound (sebundle-bell sounds))
						 (bullet-clrflags blt (bltflags uncancelable))
						 (linear-step-forever facing speed blt))
					   (loop (fl+ facing turn-dir))))))
			  (bullet-addflags (bltflags uncancelable)))))))

(define (chapter6 task)
  (set! current-chapter 6)
  (wait 30)
  (ch6-sswave -90.0 150.0 'pellet-orange 'small-ball-red)
  (wait 30)
  (ch6-sswave 90.0 150.0 'pellet-yellow 'small-ball-orange)
  (wait 30)
  (ch6-sswave -20.0 120.0 'pellet-cyan 'small-ball-blue)
  (wait 30)
  (ch6-sswave 20.0 120.0 'pellet-red 'small-ball-magenta)
  (wait 120)
  ;; last wave a bit more special
  (raylib:play-sound (sebundle-shoot0 sounds))
  (-> (cb)
	  (cbcount 20 3)
	  (cbang 0.0 10.0)
	  (cbspeed 2.0 3.5)
	  (cbshoot 0.0 100.0
		(lambda (layer in-layer speed facing)
		  (-> (spawn-bullet
			   (if (fxeven? layer) 'music-green 'rest-cyan)
			   0.0 100.0 10
			   (lambda (blt)
				 (define initial-facing facing)
				 (define turn-dir (torad (if (fxeven? layer) -4.0 4.0)))
				 (raylib:play-sound (sebundle-oldvwoopslow sounds))
				 (let loop ([facing initial-facing])
				   (bullet-facing-set! blt facing)
				   (linear-step facing speed blt)
				   (yield)
				   (if (fl> (flabs (fl- facing initial-facing)) tau)
					   (begin
						 (cancel-bullet blt #t)
						 (when (and (zero? layer)
									(zero? in-layer))
						   (-> (fb)
							   (fbcounts 7 7)
							   (fbang 0.0 5.0)
							   (fbspeed 5.0 7.0)
							   (fbshootez 0.0 100.0 'pellet-red 5 #f))))
					   (loop (fl+ facing turn-dir))))))
			  (bullet-addflags (bltflags uncancelable))))))
  (wait-until (thunk (>= frames 6725)))
  (chapter7 task))

(define (ch7-med-fairy flip task enm)
  (define points (vector (vec2 -200.0 250.0) (vec2 49.0 215.0)
						 (vec2 192.0 85.0) (vec2 0.0 26.0)
						 (vec2 -138.0 108.0)
						 (vec2 -70.0 146.0) (vec2 200.0 210.0)))
  (spawn-subtask "shoot"
	(lambda (task)
	  (let loop ([wave 0])
		(do [(i 0 (1+ i))]
			[(= i 5)]
		  (-> (cb)
			  (cbspeed 3.0)
			  (cbcount 12)
			  (cbang (inexact (+ (* 20 wave) (* 5 i))))
			  (cbshootenm enm 'heart-red 2 #f
						 (lambda (facing speed blt)
						   (bullet-facing-set! blt facing)
						   (dotimes 5
							 (linear-step facing speed blt)
							 (yield))
						   (wait 30)
						   (raylib:play-sound (sebundle-bell sounds))
						   (linear-step-forever facing speed blt))))
		  (spawn-bullet 'big-star-magenta
						(enm-x enm) (enm-y enm)
						2 (lambda (blt)
							(wait 35)
							(cancel-bullet blt))))
		(wait 30)
		(loop (1+ wave))))
	(constantly #t)
	task)
  (move-on-spline
   (if flip (vector-map (lambda (p) (vec2 (fl- (v2x p)) (v2y p)))
						points)
	   points)
   (lambda (_seg) (values values 120))
   enm)
  (delete-enemy enm))

(define (chapter7 task)
  (set! current-chapter 7)
  (wait 50)
  (spawn-enemy (enmtype big-fairy) -200.0 250.0 800 (curry ch7-med-fairy #f)
			   ten-point)
  (wait 370)
  (spawn-enemy (enmtype big-fairy) 200.0 250.0 800 (curry ch7-med-fairy #t)
			   ten-point)
  (wait-until (thunk (>= frames 7497)))
  (chapter8 task))

(define (ch8-bigfairy flip task enm)
  (define (shoot task)
	(wait 20)
	(raylib:play-sound (sebundle-shortcharge sounds))
	(wait 60)
	(let loop ([ang 0.0])
	  (-> (cb)
		  (cbabsolute-aim)
		  (cbang ang)
		  (cbcount 4)
		  (cbspeed 3.0)
		  (cboffset 25.0)
		  (cbshootenm enm (if flip 'kunai-orange 'kunai-red) 2 #f))
	  (yield)
	  (loop
	   (fl+ ang (fl* 12.0 (flcos (torad (inexact frames)))
				   (flsin (torad (inexact frames))))))))
  (spawn-subtask "shoot" shoot (constantly #t) task)
  (move-on-spline
   (if flip
	   (vector (vec2 200.0 200.0)
			   (vec2 101.0 80.0)
			   (vec2 -130.0 222.0)
			   (vec2 -200.0 -20.0))
	   (vector (vec2 -200.0 200.0)
			   (vec2 -101.0 80.0)
			   (vec2 130.0 222.0)
			   (vec2 200.0 -20.0)))
   (lambda (_seg) (values values 480))
   enm)
  (delete-enemy enm))

(define (chapter8 task)
  (define colors '#((red-fairy . big-star-red)
					(green-fairy . big-star-green)
					(blue-fairy . big-star-blue)
					(yellow-fairy . big-star-yellow)))
  (set! current-chapter 8)
  (let* ([first (-> (spawn-enemy (enmtype medium-blue-fairy)
				   -200.0 200.0 1200 (curry ch8-bigfairy #f)
				   '((point . 20)))
					(enm-addflags (enmflags nocollide)))]
		 [second (begin
				   (wait 240)
				   (-> (spawn-enemy (enmtype medium-red-fairy)
									200.0 200.0 1200 (curry ch8-bigfairy #t)
									'((point . 20)))
					   (enm-addflags (enmflags nocollide))))])
	(wait-until (thunk (and (not (vector-index first live-enm))
							(not (vector-index second live-enm)))))
	(interval-loop-while 30 (< frames 8070)
	  (let ([pair (vrand colors game-rng)])
		(spawn-enemy
		 (car pair)
		 (centered-roll game-rng 160.0) -20.0 70
		 (lambda (task enm)
		   (ease-to ease-out-quad
					(+ (enm-x enm) (centered-roll game-rng 20.0))
					(+ (enm-y enm) (+ 50.0 (roll game-rng 20)))
					30 enm)
		   (dotimes 3
			 (-> (fb)
				 (fbcounts 3 5)
				 (fbspeed 2.0 4.0)
				 (fbang 0.0 20.0)
				 (fbshootenm enm (cdr pair) 5 (sebundle-shoot0 sounds)))
			 (wait 30))
		   (ease-to values
					(+ (enm-x enm) (centered-roll game-rng 20.0))
					-20.0
					30 enm)
		   (delete-enemy enm))))))
  (wait-until (thunk (>= frames 8284)))
  (chapter9 task))

(define (ch9-w1-left blttype points task enm)
  (spawn-subtask "shoot"
	(lambda (task)
	  (interval-loop 15
		(-> (fb)
			(fbcounts 1 3)
			(fbspeed 4.0 5.0)
			(fbshootenm enm blttype 2 (sebundle-shoot0 sounds)))))
	(constantly #t)
	task)
  (loop-until (flpositive? (enm-x enm))
	(linear-step-enm 0.0 4.0 enm))
  (move-on-spline points (lambda (_seg) (values values 60)) enm)
  (loop-until (fl< (enm-x enm) -200.0)
	(linear-step-enm pi 4.0 enm))
  (delete-enemy enm))

(define (ch9-w1-right blttype points task enm)
  (spawn-subtask "shoot"
	(lambda (task)
	  (interval-loop 15
		(-> (fb)
			(fbcounts 1 3)
			(fbspeed 4.0 5.0)
			(fbshootenm enm blttype 2 (sebundle-shoot0 sounds)))))
	(constantly #t)
	task)
  (loop-until (flnegative? (enm-x enm))
	(linear-step-enm pi 4.0 enm))
  (move-on-spline points (lambda (_seg) (values values 60)) enm)
  (loop-until (fl> (enm-x enm) 200.0)
	(linear-step-enm 0.0 4.0 enm))
  (delete-enemy enm))

(define (ch9-w2 task enm)
  (enm-superarmor-set! enm 60)
  (ease-to values (enm-x enm) (+ 140.0 (enm-y enm)) 90 enm)
  (let ([start-frames frames]
		[x (enm-x enm)]
		[y (enm-y enm)])
	(interval-loop-while 30 (fx< (fx- frames start-frames) 240)
	  (raylib:play-sound (sebundle-shoot0 sounds))
	  (letrec ([facing (facing-player x y)]
			   [center-blt
				(spawn-bullet
				 'big-star-orange x y 5
				 (lambda (blt)
				   (do [(i 0 (fx1+ i))]
					   [(fx= i 241)]
					 (linear-step facing 5.0 blt)
					 (let ([r (flmin (fl* (inexact i) 1.5) 30.0)])
					   (position-bullets-around (bullet-x center-blt)
												(bullet-y center-blt)
												r 0.0 ring-blts))
					 (yield))
				   (delete-bullet blt)))]
			   [ring-blts (map
						   (lambda (_)
							 (spawn-bullet
							  'small-star-yellow x y 5
							  (lambda (blt)
								(wait-until
								 (thunk (not (vector-index center-blt live-bullets))))
								(delete-bullet blt))))
						   (iota 10))])
		(void))))
  (ease-to ease-in-quad (enm-x enm) -20.0 60 enm)
  (delete-enemy enm))

(define (chapter9 task)
  (set! current-chapter 9)
  (dotimes 10
	(spawn-enemy (enmtype red-fairy) -200.0 117.0 90
				 (curry ch9-w1-left
						'arrowhead-red
						(vector (vec2 0.0 116.0) (vec2 81.0 122.0)
								(vec2 110.0 257.0) (vec2 0.0 265.0))))
	(wait 10))
  (wait 100)
  (dotimes 10
	(spawn-enemy (enmtype green-fairy) 200.0 117.0 90
				 (curry ch9-w1-right
						'arrowhead-green
						(vector (vec2 0.0 116.0) (vec2 -81.0 122.0)
								(vec2 -110.0 257.0) (vec2 0.0 265.0))))
	(wait 10))

  (wait 120)
  (dotimes 10
	(spawn-enemy (enmtype blue-fairy) -200.0 265.0 90
				 (curry ch9-w1-left
						'arrowhead-blue
						(vector (vec2 0.0 265.0) (vec2 110.0 257.0)
								(vec2 81.0 122.0) (vec2 0.0 116.0))))
	(wait 10))
  (wait 100)
  (dotimes 10
	(spawn-enemy (enmtype yellow-fairy) 200.0 265.0 90
				 (curry ch9-w1-right
						'arrowhead-yellow
						(vector (vec2 0.0 265.0) (vec2 -110.0 257.0)
								(vec2 -81.0 122.0) (vec2 0.0 116.0))))
	(wait 10))
  (let ([drops '((point . 15))]
		[on-death (let ([killed (box 0)])
					(lambda ()
					  (set-box! killed (add1 (unbox killed)))
					  (when (= (unbox killed) 3)
						(spawn-drops '((bomb-frag . 1)) 0.0 110.0))))])
	(spawn-enemy (enmtype big-fairy) -90.0 -20.0 400 ch9-w2 drops on-death)
	(spawn-enemy (enmtype big-fairy) 0.0 -30.0 400 ch9-w2 drops on-death)
	(spawn-enemy (enmtype big-fairy) 90.0 -20.0 400 ch9-w2 drops on-death))
  (wait-until (thunk (>= frames 9392)))
  (chapter10 task))

(define (ch10-w1 right-side task enm)
  (define dest-x (if right-side
					 (- +playfield-min-x+ 20.0)
					 (+ +playfield-max-x+ 20.0)))
  (define dest-y (+ (enm-y enm)
					(centered-roll game-rng 80.0)))
  (define (shoot task)
	(interval-loop 10
	  (let ([color (vrand '#(music-cyan music-blue music-green) game-rng)])
		(-> (cb)
			(cbcount 16)
			(cbspeed 3.0)
			(cbabsolute-aim)
			(cbshoot (enm-x enm) (enm-y enm)
			  (lambda (layer in-layer speed facing)
				(spawn-bullet color (enm-x enm) (enm-y enm) 5
							  (curry linear-step-with-bounce facing speed))
				(wait 2)))))))
  (spawn-subtask "shoot" shoot (constantly #t) task)
  (ease-to values dest-x dest-y 180 enm)
  (delete-enemy enm))

(define (linear-step-with-bounce facing speed blt)
  (let loop ()
	(let ([ox (bullet-x blt)]
		  [oy (bullet-y blt)])
	  (linear-step facing speed blt)
	  (yield)
	  (let ([nx (bullet-x blt)]
			[ny (bullet-y blt)])
		(cond
		 [(> ny +playfield-max-y+)
		  (linear-step-forever facing speed blt)]
		 [(< ny +playfield-min-y+)
		  (let ([xv (flcos facing)]
				[yv (flsin facing)])
			(linear-step-forever (flatan (fl- yv) xv) speed blt))]
		 [(or (< nx +playfield-min-x+)
			  (> nx +playfield-max-x+))
		  (let ([xv (flcos facing)]
				[yv (flsin facing)])
			(linear-step-forever (flatan yv (fl- xv)) speed blt))])
		)
	  (loop))))

(define (ch10-w2 type right-side task enm)
  (ease-to values
		   (+ (enm-x enm) (if right-side -50.0 50.0))
		   (enm-y enm) 20 enm)
  (-> (fb)
	  (fbcounts 1 10)
	  (fbspeed 1.0 (fl+ 5.5 (centered-roll game-rng 0.7)))
	  (fbabsolute-aim)
	  (fbang (if right-side 180.0 0.0) 0.0)
	  (fbshootenm enm type
				 2 (sebundle-shoot0 sounds)
				 (lambda (facing speed blt)
				   (dotimes 60
					 (linear-step facing speed blt)
					 (yield))
				   (linear-step-forever (fl/ pi 2.0) 3.0 blt))))
  (wait 60)
  (spawn-subtask "rings"
	(lambda (task)
	  (interval-loop 50
		(-> (cb)
			(cbcount 12)
			(cbspeed 4.0)
			(cbabsolute-aim)
			(cbang (inexact (roll game-rng 360)) 0.0)
			(cbshootenm enm 'small-ball-yellow 2 (sebundle-bell sounds)))))
	(constantly #t) task)
  (ease-to values (if right-side -200.0 200.0)
		   (enm-y enm)
		   300 enm)
  (delete-enemy enm))

(define (ch10-w3 task enm)
  (enm-superarmor-set! enm 60)
  (ease-to values 0.0 100.0 60 enm)
  (spawn-subtask "hearts"
	(lambda (task)
	  (interval-loop 30
		(-> (cb)
			(cbcount 12)
			(cbspeed 5.0)
			(cbshootenm enm 'heart-red 2 #f linear-step-forever))))
	(constantly #t)
	task)
  (spawn-subtask "shoot"
	(lambda (task)
	  (define iters 5)
	  (define angper 8.0)
	  (let loop ([sign 1]
				 [start-ang 0.0])
		(do [(i 0 (fx1+ i))]
			[(= i iters)]
		  (-> (cb)
			  (cbabsolute-aim)
			  (cbcount 5)
			  (cbang (fl+ start-ang (fl* (inexact sign) (inexact i) angper)))
			  (cbspeed 3.0)
			  (cbshootenm enm 'big-star-magenta 2 (sebundle-shoot0 sounds)
						 (lambda (facing speed blt)
						   (linear-step-decelerate facing speed -0.1 blt)
						   (wait 10)
						   (linear-step-accelerate-forever facing 0.0 0.7 6.0 blt))))
		  (wait 5))
		(wait 5)
		(loop (- sign)
			  (fl+ start-ang
				   ;; start where previous wave ended, plus a bit more
				   (* sign angper iters)
				   (* sign 5.0)))))
	(constantly #t)
	task)
  (wait 190)
  (ease-to values 0.0 -20.0 70 enm)
  (delete-enemy enm))

(define (ch10-w4 task enm)
  (define (shoot task)
	(wait 10)
	(interval-loop 5
	  (-> (fb)
		  (fbspeed 5.0)
		  (fbcounts 2 1)
		  (fbang 0.0 45.0)
		  (fbshootenm enm 'music-red 2 #f))
	  (-> (fb)
		  (fbspeed 5.0)
		  (fbcounts 1 3)
		  (fbshootenm enm 'music-blue 2 #f))))
  (spawn-subtask "shoot" shoot (constantly #t) task)
  (ease-to values 200.0 (enm-y enm) 100 enm)
  (delete-enemy enm))

(define (ch10-w5 right-side task enm)
  (define types '(fixed-laser-red
				  fixed-laser-orange fixed-laser-blue
				  fixed-laser-magenta))
  (ease-to values
		   (+ (enm-x enm)
			  (if right-side -50.0 50.0))
		   (enm-y enm)
		   20 enm)
  (for-each
   (lambda (type)
	(raylib:play-sound (sebundle-laser sounds))
	(spawn-laser type
				 (enm-x enm) (enm-y enm)
				 (facing-player (enm-x enm) (enm-y enm))
				 550.0 5.0 5 30
				 (lambda (_blt) (wait 40)))
	(wait 25))
   types)
  (wait-until (thunk (>= frames 10872)))
  (for-each
   (lambda (type)
	 (-> (fb)
		 (fbcounts 1 5)
		 (fbspeed 4.0 7.0)
		 (fbshootenm enm type 2 (sebundle-bell sounds)))
	 (-> (cb)
		 (cbcount 16 3)
		 (cbspeed 4.0 7.0)
		 (cbshootenm enm 'small-ball-blue 2 #f))
	 (wait 48))
   '(bubble-red bubble-orange bubble-blue bubble-magenta)))

(define (chapter10 task)
  (set! current-chapter 10)
  (wait 70)
  (spawn-enemy 'medium-red-fairy -192.0 100.0 270 (curry ch10-w1 #f)
			   '((point . 5) (bomb-frag . 1)))
  (spawn-enemy 'medium-red-fairy 192.0 100.0 270 (curry ch10-w1 #t)
			   '((point . 5) (bomb-frag . 1)))
  (wait 300)
  (let loop ([y 50.0]
			 [right-side #f]
			 [types '(knife-red knife-orange knife-blue knife-magenta knife-yellow)])
	(spawn-enemy (if right-side 'red-fairy 'blue-fairy)
				 (if right-side 200.0 -200.0) y
				 100 (curry ch10-w2 (car types) right-side)
				 ten-point)
	(wait 30)
	(unless (null? (cdr types))
	  (loop (fl+ y 40.0) (not right-side) (cdr types))))

  (wait 300)
  (spawn-enemy 'big-fairy 0.0 -20.0 1500 ch10-w3 '((point . 10)))
  (wait 240)
  (dotimes 10
	(spawn-enemy 'red-fairy -200.0 200.0 100 ch10-w4)
	(wait 10))
  (wait 100)
  (-> (spawn-enemy 'green-fairy
				   -200.0 50.0
				   150 (curry ch10-w5 #f))
	  (enm-superarmor-set! 90))
  (-> (spawn-enemy 'green-fairy
				   200.0 50.0
				   150 (curry ch10-w5 #t))
  	  (enm-superarmor-set! 90))
  (wait 100)
  (-> (spawn-enemy 'green-fairy
				   -200.0 130.0
				   150 (curry ch10-w5 #f))
	  (enm-superarmor-set! 50))
  (-> (spawn-enemy 'green-fairy
				   200.0 130.0
				   150 (curry ch10-w5 #t))
	  (enm-superarmor-set! 50))
  (wait-until (thunk (>= frames 10960)))
  (chapter11 task))

(define (midboss2-control task enm)
  (define bossinfo (enm-extras enm))
  (define keep-running
	(lambda () (positive? (bossinfo-remaining-timer bossinfo))))
  (ease-to values 0.0 100.0 20 enm)
  (wait 50)

  (vector-for-each-truthy
   (lambda (enm)
	 (unless (is-boss? enm)
	   (kill-enemy enm)))
   live-enm)
  (autocollect-all-items)
  (declare-spell enm 1)
  (enm-addflags enm (enmflags invincible))
  (cancel-all #f)
  (wait 60)
  (spawn-subtask "fan"
	(lambda (task)
	  (wait 30)
	  (interval-loop 60
		(-> (fb)
			(fbang 0.0 25.0)
			(fbcounts 5)
			(fbspeed 4.0)
			(fbshootenm enm 'medium-ball-red 10 #f))))
	keep-running task)
  (spawn-subtask "circle"
	(lambda (task)
	  (define dang (torad 10.0))
	  (define cx player-x)
	  (define cy player-y)
	  (define dist 120.0)
	  (let loop ([ang 0.0])
		(let* ([x (fl+ cx (fl* dist (flcos ang)))]
			   [y (fl+ cy (fl* dist (flsin ang)))]
			   [ang1 (flatan (- cy y) (- cx x))]
			   [x2 (fl+ cx (fl* dist (flcos (fl+ ang pi))))]
			   [y2 (fl+ cy (fl* dist (flsin (fl+ ang pi))))]
			   [ang2 (flatan (- cy y2) (- cx x2))])
		  (raylib:play-sound (sebundle-bell sounds))
		  (spawn-bullet 'butterfly-magenta x y 5
						(curry linear-step-accelerate-forever
							   ang1 0.5 0.07 2.0))
		  (-> (fb)
			  (fbcounts 5 2)
			  (fbabsolute-aim)
			  (fbang (todeg (fl+ ang1 (fl* 3.0 (fl/ pi 4.0)))) 30.0)
			  (fbspeed 3.0 5.0)
			  (fbshootez x y 'music-red 5 #f))
		  (spawn-bullet 'butterfly-cyan x2 y2 5
						(curry linear-step-accelerate-forever
							   ang2
							   0.5 0.07 2.0))
		  (-> (fb)
			  (fbcounts 5 2)
			  (fbabsolute-aim)
			  (fbang (todeg (fl+ ang2 (fl* 3.0 (fl/ pi 4.0)))) 30.0)
			  (fbspeed 3.0 5.0)
			  (fbshootez x2 y2 'music-orange 5 #f))
		  (wait 5)
		  (loop (fl+ ang dang)))))
	keep-running
	task)
  (wait-while keep-running)
  (common-spell-postlude bossinfo enm)
  (common-boss-postlude bossinfo enm #t)
  (wait 75)
  (ease-to values -100.0 -100.0 20 enm)
  (delete-enemy enm))

(define (chapter11 task)
  (set! current-chapter 11)
  (let ([enm (spawn-enemy (enmtype boss-doremi) 100.0 -100.0 500 midboss2-control
						  '((life . 1) (point . 50))
						  (thunk #f))]
		[bossinfo (blank-bossinfo "Harukaze Doremi" #xff7fbcff)])
	(bossinfo-healthbars-set!
	 bossinfo
	 (immutable-vector (make-healthbar -1 0.0 #xf5f5f5ff #x808080ff)))
	(enm-extras-set! enm bossinfo))
  (wait-until (thunk (>= frames 11785)))
  (chapter12 task))

(define (ch12-small-fairy right-side task enm)
  (spawn-subtask "shoot"
	(lambda (task)
	  (interval-loop 15
		(-> (cb)
			(cbcount 12)
			(cbspeed 4.5)
			(cbshootenm enm 'rice-cyan 2 #f))))
	(constantly #t)
	task)
  (move-on-spline
   (if right-side
	   (vector (vec2 (enm-x enm) (enm-y enm))
			   (vec2 -5.0 168.0) (vec2 -133.0 146.0)
			   (vec2 -210.0 (fl+ 61.0 (centered-roll game-rng 30.0))))
	   (vector (vec2 (enm-x enm) (enm-y enm))
			   (vec2 5.0 168.0) (vec2 133.0 146.0)
			   (vec2 210.0 (fl+ 61.0 (centered-roll game-rng 30.0)))))
   (lambda (_seg) (values values 100))
   enm)
  (delete-enemy enm))

(define ch12-sswave
  (case-lambda
	([x y final-way final-wideness even-type odd-type]
	 (ch12-sswave x y final-way final-wideness even-type odd-type #f #f))
	([x y final-way final-wideness even-type odd-type force-aim-x force-aim-y]
	 (-> (cb)
		 (cbcount 20 3)
		 (cbang 0.0 10.0)
		 (cbspeed 2.0 3.5)
		 (cbshoot x y
		   (lambda (layer in-layer speed facing)
			 (-> (spawn-bullet
				  (if (fxeven? layer) even-type odd-type)
				  x y 10
				  (lambda (blt)
					(define initial-facing facing)
					(define turn-dir (torad (if (fxeven? layer) -5.5 5.5)))
					(let loop ([facing initial-facing])
					  (bullet-facing-set! blt facing)
					  (linear-step facing speed blt)
					  (yield)
					  (if (fl> (flabs (fl- facing initial-facing)) tau)
						  (begin
							(cancel-bullet blt #t)
							(when (and (zero? layer)
									   (zero? in-layer))
							  (-> (fb)
								  (fbcounts final-way 7)
								  (fbabsolute-aim)
								  (fbang
								   (todeg
									(flatan (fl- (or force-aim-y player-y) y)
											(fl- (or force-aim-x player-x) x)))
								   final-wideness)
								  (fbspeed 5.0 7.0)
								  (fbshootez x y 'pellet-white 5
											 (sebundle-shoot0 sounds)))))
						  (loop (fl+ facing turn-dir))))))
				 (bullet-addflags (bltflags uncancelable)))))))))

(define (chapter12 task)
  (set! current-chapter 12)
  (wait 50)
  (dotimes 60
	(spawn-enemy 'red-fairy (- -20.0 (inexact (roll game-rng 100))) -10.0
				 20 (curry ch12-small-fairy #f) five-point-items)
	(wait 5))
  (wait 75)
  (dotimes 25
	(spawn-enemy 'red-fairy (+ 20.0 (inexact (roll game-rng 100))) -10.0
				 20 (curry ch12-small-fairy #t) five-point-items)
	(wait 5))
  (wait 60)
  (wait 90)
  (ch12-sswave -160.0 160.0 3 8.0 'pellet-magenta 'music-red)
  (ch12-sswave 160.0 160.0 3 8.0 'pellet-magenta 'music-red)
  (ch12-sswave 0.0 120.0 3 8.0 'pellet-magenta 'music-red)
  (wait 90)
  (ch12-sswave -160.0 32.0 5 4.0 'pellet-yellow 'music-orange)
  (ch12-sswave 160.0 32.0 5 4.0 'pellet-yellow 'music-orange)
  (ch12-sswave -160.0 416.0 5 4.0 'pellet-yellow 'music-orange)
  (ch12-sswave 160.0 416.0 5 4.0 'pellet-yellow 'music-orange)
  (wait 70)
  (ch12-sswave -160.0 224.0 6 10.0 'pellet-cyan 'music-blue 0.0 224.0)
  (ch12-sswave 160.0 224.0 6 10.0 'pellet-cyan 'music-blue 0.0 224.0)
  (ch12-sswave 0.0 32.0 6 10.0 'pellet-cyan 'music-blue 0.0 224.0)
  (ch12-sswave 0.0 416.0 6 10.0 'pellet-cyan 'music-blue 0.0 224.0)
  (wait-until (thunk (>= frames 13000)))
  (chapter13 task))

(define (chapter13 task)
  (set! current-chapter 13)
  (wait 180)
  (stage-ctx-dialogue-set!
   current-stage-ctx
   (with-input-from-file "assets/dialogue/prebattle.dat" read))
  (stage-ctx-dialogue-idx-set! current-stage-ctx 0)
  (wait-until (thunk (not (stage-ctx-dialogue current-stage-ctx))))
  (let-values ([(doremi hazuki aiko) (find-bosses)])
	(group-non1 task doremi hazuki aiko)))
