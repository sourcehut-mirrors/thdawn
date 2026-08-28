;; Copyright (C) 2025 Vincent Lee; GPL-3.0-or-later
(define (make-non-healthbar)
  (make-healthbar -1 0 #xf5f5f5ff #x808080ff))

(define (adjust-bars-non healthbars)
  ;; add upcoming nonspell (replacing the spell that just expired)
  (vector-set! healthbars (sub1 (vlen healthbars))
			   (make-non-healthbar))
  ;; make upcoming spell bigger and remove its padding
  (let ([sp (vnth healthbars (- (vlen healthbars) 2))])
	(healthbar-width-set! sp 25)
	(healthbar-post-padding-set! sp 0))
  healthbars)

(define (base-healthbars)
  (vector
   ;; final
   (make-healthbar 4 1 #xffd700ff #xdaa520ff)
   ;; survival
   (make-healthbar 4 1 #xffd700ff #xdaa520ff)
   ;; aiko sp2
   (make-healthbar 4 1 aiko-color #x008b8bff)
   ;; hazuki sp2
   (make-healthbar 4 1 hazuki-color #xf4a460ff)
   ;; doremi sp2
   (make-healthbar 4 1 doremi-color #xba55d3ff)
   ;; group sp2
   (make-healthbar 4 1 #xffd700ff #xdaa520ff)
   ;; aiko sp1
   (make-healthbar 4 1 aiko-color #x008b8bff)
   ;; hazuki sp1
   (make-healthbar 4 1 hazuki-color #xf4a460ff)
   ;; doremi sp1
   (make-healthbar 4 1 doremi-color #xba55d3ff)
   ;; group sp1
   (make-healthbar 25 0 #xffd700ff #xdaa520ff)))

(define (group-non1 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (define (keep-running)
	(and (fxpositive? (bossinfo-remaining-timer bossinfo))
		 (fxpositive? (enm-health doremi))))
  (set! current-chapter 14)
  (play-music (musbundle-naisho-yo-ojamajo music))
  (bossinfo-healthbars-set!
   bossinfo
   (vector-add (base-healthbars) (make-non-healthbar)))
  (enm-redirect-damage-set! hazuki doremi)
  (enm-redirect-damage-set! aiko doremi)
  (declare-nonspell doremi 1800 4800)
  (wait 100)
  (spawn-subtask "main"
	(λ (task)
	  (interval-loop 60
		(-> (cb)
			(cbcount 36 4)
			(cbang 0.0 5.0)
			(cbspeed 2.0 3.0)
			(cbshootenm doremi 'heart-magenta 5 (sebundle-shoot0 sounds)))
		(parameterize ([ovr-uncancelable #t])
		  (spawn-bullet 'glow-orb-orange (ex hazuki) (ey hazuki) 0
						(λ (task blt)
						  (spawn-subtask "subshoot"
							(λ (task)
							  (wait 44)
							  (wait 48)
							  (-> (fb)
								  (fbcount 5 5)
								  (fbang 0.0 18.0)
								  (fbspeed 3.5 4.5)
								  (fbshootez 'music-orange
											 (bx blt) (by blt)
											 2 (sebundle-bell sounds)))
							  (cancel-bullet blt #t))
							task)
						  (linear-step-forever hpi 2.5 task blt)))
		  (spawn-bullet 'glow-orb-cyan (ex aiko) (ey aiko) 0
						(λ (task blt)
						  (spawn-subtask "subshoot"
							(λ (task)
							  (wait 44)
							  (wait 48)
							  (wait 12)
							  (wait 12)
							  (-> (fb)
								  (fbcount 5 5)
								  (fbang 0.0 18.0)
								  (fbspeed 3.5 4.5)
								  (fbshootez 'music-cyan (bx blt) (by blt)
											  2 (sebundle-bell sounds)))
							  (cancel-bullet blt #t))
							task)
						  (linear-step-forever hpi 2.5 task blt))))
		(wait 44)
		(-> (cb)
			(cbcount 36 4)
			(cbang 0.0 5.0)
			(cbspeed 2.0 3.0)
			(cbshootenm doremi 'heart-blue 5 (sebundle-shoot0 sounds)))
		(wait 48)
		(wait 12)
		(wait 12)
		(wait 12)))
	task keep-running)
  (wait-while keep-running)
  (common-nonspell-postlude bossinfo doremi #t)
  (group-sp1 task doremi hazuki aiko))


(define (group-sp1 task doremi hazuki aiko)
  (define y-radius 20.0)
  (define top-y (fl- +right-boss-y+ y-radius))
  (define left-x (fl- +left-boss-x+ 30.0))
  (define right-x (fl+ +right-boss-x+ 30.0))
  (define x-radius (fl- right-x left-x))
  (define bossinfo (enm-extras doremi))
  (define (keep-running)
	(and (fxpositive? (bossinfo-remaining-timer bossinfo))
		 (fxpositive? (enm-health doremi))))
  (set! current-chapter 15)
  (enm-redirect-damage-set! hazuki doremi)
  (enm-redirect-damage-set! aiko doremi)
  (declare-spell doremi 2)

  (spawn-subtask "hazuki move"
	(λ (task)
	  (ease-to ease-out-cubic left-x (ey hazuki) 60 hazuki)
	  (wait 120)
	  (let ([start-time frames])
		(interval-loop 1
		  (let* ([t (/ (- frames start-time) 20)]
				 [mul (fl+ 0.5 (fl* 0.5 (flsin (inexact t))))])
			(enm-y-set! hazuki (fl+ top-y (fl* 2.0 y-radius mul))))
		  (let* ([t (/ (- frames start-time) 40)]
				 [mul (fl- 1.0 (fl+ 0.5 (fl* 0.5 (flcos (inexact t)))))])
			(enm-x-set! hazuki (fl+ left-x (fl* mul x-radius)))))))
	task keep-running)
  (spawn-subtask "aiko move"
	(λ (task)
	  (ease-to ease-out-cubic right-x (ey aiko) 60 aiko)
	  (wait 120)
	  (let ([start-time frames])
		(interval-loop 1
		  (let* ([t (/ (- frames start-time) 20)]
				 [mul (fl+ 0.5 (fl* 0.5 (flsin (inexact t))))])
			(enm-y-set! aiko (fl+ top-y (fl* 2.0 y-radius mul))))
		  (let* ([t (/ (- frames start-time) 40)]
				 [mul (fl+ 0.5 (fl* 0.5 (flcos (inexact t))))])
			(enm-x-set! aiko (fl+ left-x (fl* mul x-radius)))))))
	task keep-running)
  (spawn-subtask "hazuki shoot"
	(λ (task)
	  (wait 180)
	  (spawn-subtask "rings"
		(λ (task)
		  (interval-loop 90
			(-> (cb)
				(cbcount 32)
				(cbspeed 4.5)
				(cbshootenm hazuki 'music-orange 2 #f))))
		task)
	  (interval-loop 20
		(do [(i 0 (add1 i))]
			[(= i 7)]
		  (raylib:play-sound (sebundle-shootsoft sounds))
		  (spawn-bullet 'glow-ball-cyan (ex hazuki) (ey hazuki) 5
						(curry linear-step-forever hpi
							   (fl+ 2.5 (fl* (fx2fl i) 0.11))))
		  (wait 6))))
	task keep-running)
  (spawn-subtask "aiko shoot"
	(λ (task)
	  (wait 180)
	  (spawn-subtask "rings"
		(λ (task)
		  (interval-loop 90
			(-> (cb)
				(cbcount 32)
				(cbspeed 4.5)
				(cbshootenm aiko 'music-blue 2 (sebundle-bell sounds)))))
		task)
	  (interval-loop 20
		(do [(i 0 (add1 i))]
			[(= i 7)]
		  (raylib:play-sound (sebundle-shootsoft sounds))
		  (spawn-bullet 'glow-ball-blue (ex aiko) (ey aiko) 5
						(curry linear-step-forever hpi
							   (fl+ 2.5 (fl* (fx2fl i) 0.11))))
		  (wait 6))))
	task keep-running)
  (wait 60)
  (dotimes 2
	(raylib:play-sound (sebundle-shortcharge sounds))
	(wait 60))
  (raylib:play-sound (sebundle-release sounds))
  (spawn-subtask "doremi shoot"
	(λ (task)
	  (interval-loop 70
		(-> (fb)
			(fbcount 3 5)
			(fbspeed 4.0 5.5)
			(fbang 0.0 8.0)
			(fbshootenm doremi 'butterfly-red 2 (sebundle-shoot0 sounds)))))
	task keep-running)
  (wait-while keep-running)
  (common-spell-postlude bossinfo doremi)
  (doremi-non1 task doremi hazuki aiko))

(define (doremi-non1 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (define (keep-running)
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi))))
  (set! current-chapter 16)
  (wait 90)
  (adjust-bars-non (bossinfo-healthbars bossinfo))
  (spawn-subtask "hazuki leave"
	(λ (_)
	  (ease-to ease-out-cubic (flcopysign 100.0 (ex hazuki)) -100.0 60 hazuki)
	  (delete-enemy hazuki))
	task)
  (spawn-subtask "aiko leave"
	(λ (_)
	  (ease-to ease-out-cubic (flcopysign 100.0 (ex aiko)) -100.0 60 aiko)
	  (delete-enemy aiko))
	task)
  (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+ 60 doremi)
  (declare-nonspell doremi 1800 10000)
  (spawn-subtask "main"
	(λ (task)
	  (define start-time frames)
	  (define (rage)
		(or (fx< (bossinfo-remaining-timer bossinfo) 600)
			(fx< (enm-health doremi) 4000)))
	  (spawn-subtask "ring"
		(λ (task)
		  (wait 400)
		  (interval-loop (if (rage) 30 60)
		    (-> (cb)
				(cbcount 12)
				(cbspeed (if (rage) 3.25 2.25))
				(cbshoot (ex doremi) (ey doremi)
				  (λ (layer in-layer speed facing)
					(define-values (x y) (dist-away (ex doremi) (ey doremi)
													facing 40.0))
					(raylib:play-sound (sebundle-bell sounds))
					(letrec* ([orb (spawn-bullet
									'medium-ball-magenta x y 8
									(λ (task blt)
									  (define realfacing (fl+ facing (torad 165.0)))
									  (loop-forever
									   (linear-step realfacing speed blt)
									   (position-bullets-around
										(bx blt) (by blt)
										20.0 0.0 ring))))]
							  [orb-index (vector-index orb live-bullets)]
							  [ring
							   (map
								(λ (_)
								  (spawn-bullet
								   'pellet-blue x y 8
								   (λ (task blt)
									 (wait-until
									  (thunk
									   (not (eq? orb (vnth live-bullets orb-index)))))
									 (delete-bullet blt))))
								(iota 10))]
							  )
					  (position-bullets-around x y 20.0 0.0 ring)))))))
		task)
	  (let loop ([i 0] [ang 90.0])
		(-> (fb)
			(fbcount 5)
			(fbspeed (if (rage) 3.0 2.5))
			(fbabsolute-aim)
			(fbang ang 12.0)
			(fboffset 20.0)
			(fbshootenm doremi 'rice-red 5
						(sebundle-shoot0 sounds)
						(λ (facing speed task blt)
						  (define left-bound (- +playfield-min-x+ 10))
						  (define right-bound (+ +playfield-max-x+ 10))
						  (loop-until
							  (or (< (bx blt) left-bound)
								  (> (bx blt) right-bound))
							(linear-step facing speed blt))
						  (if (< (bx blt) left-bound)
							  (bullet-x-set! blt (fx2fl right-bound))
							  (bullet-x-set! blt (fx2fl left-bound)))
						  (linear-step-forever facing speed task blt))))
		(wait (cond
			   [(rage) 3]
			   [(>= i 25) 5]
			   [else (vnth '#(25 25 25 25 25 15 15 15 15 15 
								 10 10 10 10 10 8 8 8 8 8 8 8 8 8 8) i)]))
		(loop (add1 i) (fl+ ang 14.0))))
	task keep-running)
  (wait-while keep-running)
  (common-nonspell-postlude bossinfo doremi)
  (doremi-sp1 task doremi))

(define (doremi-sp1 task doremi)
  (define bossinfo (enm-extras doremi))
  (define (keep-running)
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi))))
  (define (p2)
	(or (< (bossinfo-remaining-timer bossinfo) 2700)
		(< (enm-health doremi) 10000)))
  (define (p3)
	(or (< (bossinfo-remaining-timer bossinfo) 1200)
		(< (enm-health doremi) 6000)))
  (set! current-chapter 17)
  (declare-spell doremi 3)
  (wait 60)
  (raylib:play-sound (sebundle-brasscharge sounds))
  (wait 60)
  (spawn-subtask "main"
	(λ (task)
	  (spawn-subtask "rings"
		(λ (task)
		  (wait-until p3)
		  (raylib:play-sound (sebundle-shoot0 sounds))
		  (do [(i 0 (add1 i))] [#f]
			(when (even? i)
			  (-> (cb)
				  (cbcount 14)
				  (cbspeed 2.75)
				  (cbshootenm doremi 'glow-orb-red 2 #f)))
			(-> (cb)
				(cbcount 28)
				(cbspeed 2.75)
				(cbshootenm doremi 'small-ball-magenta 2 (sebundle-bell sounds)))
			(wait 60)))
		task)
	  (spawn-subtask "slow mentos"
		(λ (task)
		  (wait-until p2)
		  (raylib:play-sound (sebundle-shoot0 sounds))
		  (interval-loop 30
		    (-> (cb)
				(cbcount 8)
				(cbspeed 1.9)
				(cbshootenm doremi 'medium-ball-red 2 #f
							(λ (facing speed task blt)
							  (define winding (if (roll-bool game-rng) 1.0 -1.0))
							  (dotimes 40
								(linear-step facing speed blt)
								(yield))
							  (do [(i 0 (add1 i))]
								  [(= i 60)]
								(linear-step (fl+ facing (fl* (fx2fl i)
															  winding (torad 0.8)))
											 speed blt)
								(yield))
							  (linear-step-forever (fl+ facing (fl* 60.0 winding
																	(torad 0.8)))
												   speed task  blt))))))
		task)
	  (spawn-subtask "rain"
		(λ (task)
		  (interval-loop 10
			(let ([x (centered-roll game-rng 140.0)]
				  [facing (roll-flrange game-rng (torad 70.0) (torad 120.0))])
			  (spawn-bullet 'small-ball-orange x 0.0 5
							(curry linear-step-forever facing 2.25) 1))))
		task)
	  (let loop ([state 'center-left] [wave 0])
		(raylib:play-sound (sebundle-shoot0 sounds))
		(do [(i 0 (add1 i))]
			[(= i 12)]
		  (-> (cb)
			  (cbcount 21)
			  (cbabsolute-aim)
			  (cbang (fl+ 0.0 (fx2fl (* i (if (even? wave) 2 -2)))))
			  (cbspeed 0.4)
			  (cboffset (fx2fl (* i 12)))
			  (cbshootenm doremi 'rice-red 2 #f
						  (λ (facing speed task blt)
							(dotimes (+ 120 (* i 2))
							  (linear-step facing speed blt)
							  (yield))
							(raylib:play-sound (sebundle-bell sounds))
							(linear-step-accelerate-forever
							 facing speed 0.1 4.0 task blt))))
		  (wait 5))
		(wait 60)
		(let ([x (case state
				   [(center-left) +left-boss-x+]
				   [(left right) +middle-boss-x+]
				   [(center-right) +right-boss-x+])]
			  [y (vrand '#(125.0 90.0 55.0) game-rng)])
		  (ease-to ease-in-out-quad x y 120 doremi))
		(loop (case state
				[(center-left) 'left]
				[(left) 'center-right]
				[(right) 'center-left]
				[(center-right) 'right])
			  (add1 wave))))
	task keep-running)
  (wait-while keep-running)
  (common-spell-postlude bossinfo doremi)
  (hazuki-non1 task doremi))

(define (hazuki-non1-flower petal-type x y speed facing)
  (define ring-ang (fl* tau (roll game-rng)))
  (letrec* ([center (spawn-bullet
					 'small-ball-white x y 5
					 (λ (task blt)
					   (loop-forever
						(linear-step facing speed blt)
						(position-bullets-around (bx blt) (by blt)
													 12.0 ring-ang ring))))]
			[center-idx (vector-index center live-bullets)]
			[ring (map
				   (λ (_)
					 (spawn-bullet
					  petal-type x y 5
					  (λ (task blt)
						(wait-until
						 (thunk (not (eq? center (vnth live-bullets center-idx)))))
						(delete-bullet blt))))
				   (iota 5))])
	(position-bullets-around x y 12.0 ring-ang ring)))

(define (hazuki-non1 task doremi)
  (define bars (bossinfo-healthbars (enm-extras doremi)))
  (define _ (wait 90))
  (define hazuki
	(spawn-enemy (enmtype boss-hazuki) 100.0 -100.0 500
				 (λ (task enm)
				   (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+
							60 enm))
				 '()
				 (constantly #f)))
  (define bossinfo (blank-hazuki-bossinfo))
  (define (keep-running)
	(and (fxpositive? (bossinfo-remaining-timer bossinfo))
		 (fxpositive? (enm-health hazuki))))
  (set! current-chapter 18)
  (spawn-subtask "doremi leave"
	(λ (_)
	  (ease-to ease-out-cubic -100.0 -100.0 60 doremi)
	  (delete-enemy doremi))
	task)
  (adjust-bars-non bars)
  (bossinfo-healthbars-set! bossinfo bars)
  (enm-extras-set! hazuki bossinfo)
  (declare-nonspell hazuki 1800 7500)
  (wait 60)
  (raylib:play-sound (sebundle-longcharge sounds))
  (wait 45)
  (spawn-subtask "circle"
	(λ (task)
	  (define init-ang (centered-roll game-rng 180.0))
	  (let loop ([i 0])
		(-> (cb)
			(cbcount 15)
			(cbspeed 3.75)
			(cbabsolute-aim)
			(cbang (fl+ init-ang (fx2fl (fx* 7 i))))
			(cbshoot (ex hazuki) (ey hazuki)
			  (λ (layer in-layer speed facing)
				(define-values (x y) (dist-away (ex hazuki) (ey hazuki)
												facing 70.0))
				(hazuki-non1-flower
				 (vnth-mod '#(rice-red
							  rice-orange rice-blue rice-magenta) i)
				 x y speed facing))))
		(wait 15)
		(loop (add1 i))))
	task keep-running)
  (spawn-subtask "aimed"
	(λ (task)
	  (define (doit i right-side)
		(parameterize ([ovr-noprune #t])
		  (-> (fb)
			  (fbcount 3)
			  (fbspeed (fl- 5.0 (fl* 0.6 (fx2fl i))))
			  (fbabsolute-aim)
			  (fbang
			   (fl+ (todeg (facing-player (ex hazuki) (ey hazuki)))
					90.0
					(if right-side 180.0 0.0))
			   15.0)
			  (fbrenderprio 1)
			  (fbshootenm
			   hazuki
			   (vnth
				'#(knife-red knife-orange knife-blue knife-magenta knife-yellow) i)
			   2 (sebundle-shoot0 sounds)
			   (λ (facing speed task blt)
				 (dotimes 30
				   (linear-step facing speed blt)
				   (yield))
				 (wait 60)
				 (-> (fb)
					 (fbcount 3)
					 (fbspeed 7.5)
					 (fbang 0.0 22.5)
					 (fbrenderprio 1)
					 (fbshootez (bullet-type blt) (bx blt) (by blt)
								2 (sebundle-bell sounds)))
				 (cancel-bullet blt))))))
	  (interval-loop 50
		(do [(i 0 (add1 i))]
			[(= i 5)]
		  (doit i #f)
		  (doit i #t)
		  (wait 15))))
	task keep-running)  
  (wait-while keep-running)
  (common-nonspell-postlude bossinfo hazuki)
  (hazuki-sp1 task hazuki))

(define (hazuki-sp1-flower x y)
  (define petal-type (vrand '#(rice-red
							   rice-orange rice-blue rice-magenta)
							game-rng))
  (define ring-ang (fl* tau (roll game-rng)))
  (define start-frames frames)
  (define rev-spin (roll-bool game-rng))
  (define (cur-ang)
	(define dang (torad (flmod (* 1.2 (+ start-frames frames)) 360.0)))
	(if rev-spin (fl- ring-ang dang) (fl+ ring-ang dang)))
  (letrec* ([center (spawn-bullet
					 'small-ball-magenta x y 5
					 (λ (task blt)
					   (dotimes 85
						 (position-bullets-around (bx blt) (by blt)
												  12.0 (cur-ang) ring)
						 (yield))
					   (raylib:play-sound (sebundle-bell sounds))
					   (let ([facing (fl+ (facing-player (bx blt) (by blt))
										  (centered-roll game-rng (torad 5.0)))])
						 (loop-forever
						  (linear-step facing 3.75 blt)
						  (position-bullets-around (bx blt) (by blt)
												   12.0 (cur-ang) ring)))))]
			[center-idx (vector-index center live-bullets)]
			[ring (map
				   (λ (_)
					 (spawn-bullet
					  petal-type x y 5
					  (λ (task blt)
						(wait-until
						 (thunk (not (eq? center (vnth live-bullets center-idx)))))
						(delete-bullet blt))))
				   (iota 5))])
	(bullet-addflags center (bltflags nocanceldrop))
	(for-each (λ (blt) (bullet-addflags blt (bltflags nocanceldrop))) ring)
	center))

(define (hazuki-sp1 task hazuki)
  (define bossinfo (enm-extras hazuki))
  (define (keep-running)
	(and (fxpositive? (bossinfo-remaining-timer bossinfo))
		 (fxpositive? (enm-health hazuki))))
  (define (hurry)
	(fx< (bossinfo-remaining-timer bossinfo) 600))
  (set! current-chapter 19)
  (declare-spell hazuki 4)
  (ease-to ease-in-out-quad (ex hazuki) (fl+ (ey hazuki) 50.0) 60 hazuki)
  (raylib:play-sound (sebundle-shortcharge sounds))
  (wait 60)
  (raylib:play-sound (sebundle-release sounds))
  (spawn-subtask "main"
	(λ (task)
	  (define base-ang (fl* 360.0 (roll game-rng)))
	  (do [(i 0 (add1 i))]
		  [#f]
		(do [(j 0 (add1 j))]
			[(= j 4)]
		  (-> (cb)
			  (cbcount 7)
			  (cbabsolute-aim)
			  (cbang (fl+ base-ang (fx2fl (* 15 i)) (fx2fl (* 5 j))))
			  (cbspeed 3.0)
			  (cbshootenm
			   hazuki (if (even? j) 'glow-orb-orange 'glow-orb-magenta)
			   2 (sebundle-shoot0 sounds)
			   (λ (facing speed task blt)
				 (spawn-subtask "spawn flowers"
				   (λ (task)
					 (wait 15)
					 (dotimes 2
					   (when (fl< (by blt) 315.0)
						 (hazuki-sp1-flower (bx blt) (by blt)))
					   (wait 60)))
				   task)
				 (-> (linear-step-curve facing speed 0.0
										(torad (if (even? j) 3.0 -3.0))
										(torad 225.0) task blt)
					 (linear-step-curve speed 0.0
										(torad (if (even? j) 0.2 -0.2))
										+inf.0 task blt)))))
		  (wait (if (hurry) 15 20)))
		(wait (if (hurry) 75 90))))
	task keep-running)
  (wait-while keep-running)
  (common-spell-postlude bossinfo hazuki)
  (ease-to ease-in-out-quad (ex hazuki) (fl- (ey hazuki) 50.0) 45 hazuki)
  (aiko-non1 task hazuki))

(define (aiko-non1 task hazuki)
  (define bars (bossinfo-healthbars (enm-extras hazuki)))
  (define _ (wait 90))
  (define aiko
	(spawn-enemy (enmtype boss-aiko) 100.0 -100.0 500
				 (λ (task enm)
				   (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+
							60 enm))
				 '()
				 (constantly #f)))
  (define bossinfo (blank-aiko-bossinfo))
  (define (keep-running)
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health aiko))))
  (set! current-chapter 20)
  (spawn-subtask "hazuki leave"
	(λ (_)
	  (ease-to ease-out-cubic -100.0 -100.0 60 hazuki)
	  (delete-enemy hazuki))
	task)
  (adjust-bars-non bars)
  (bossinfo-healthbars-set! bossinfo bars)
  (enm-extras-set! aiko bossinfo)
  (declare-nonspell aiko 1800 6000)
  (wait 60)
  (spawn-subtask "atk"
	(λ (task)
	  (define init-left-ang (fx2fl (roll-range game-rng 90 150)))
	  (interval-loop 30
		(let loop ([left-ang init-left-ang]
				   [right-ang (fl- 180.0 init-left-ang)]
				   [i 0])
		  (-> (fb)
			  (fbcount 1 3)
			  (fbabsolute-aim)
			  (fbang left-ang)
			  (fbspeed 3.0 5.0)
			  (fbshootenm aiko 'yinyang-blue 5 #f linear-step-with-bounce))
		  (-> (fb)
			  (fbcount 1 3)
			  (fbabsolute-aim)
			  (fbang right-ang)
			  (fbspeed 3.0 5.0)
			  (fbshootenm aiko 'yinyang-blue 5 (sebundle-shoot0 sounds)
						  linear-step-with-bounce))
		  (if (< i 10)
			  (begin
				(wait 10)
				(loop (fl- left-ang 20.0)
					  (fl+ right-ang 20.0)
					  (add1 i)))
			  (-> (cb)
				  (cbspeed 3.8 4.75)
				  (cbcount 36 4)
				  (cbang 0.0 (if (roll-bool game-rng) 3.0 -3.0))
				  (cbshoot (ex aiko) (ey aiko)
					(λ (layer in-layer speed facing)
					  (raylib:play-sound (sebundle-bell sounds))
					  (spawn-bullet
					   (vnth '#(music-yellow music-cyan music-blue music-magenta)
							 layer)
					   (ex aiko) (ey aiko) 5
					   (curry linear-step-forever facing speed))))
				  #;(cbshootenm aiko 'music-cyan 5 (sebundle-bell sounds)))))
		(boss-standard-wander-once aiko 40 50 30)))
	task keep-running)
  (wait-while keep-running)
  (common-nonspell-postlude bossinfo aiko)
  (aiko-sp1 task aiko))

(define (aiko-sp1-round task aiko)
  (define spread-signal (box #f))
  (do [(i 0 (add1 i))]
	  [(= i 3)]
	(let ([winding (if (roll-bool game-rng) 1.0 -1.0)]
		  [colors (vnth
				   '#(#(small-star-blue small-star-cyan)
					  #(small-star-orange small-star-white)
					  #(small-star-red small-star-magenta))
				   i)]
		  [x (case i
			   [(0) (fx2fl (roll-range game-rng -115 -80))]
			   [(1) (fx2fl (roll-range game-rng 80 115))]
			   [(2) (centered-roll game-rng 50.0)])]
		  [y (vnth '#(130.0 200.0 270.0) i)])
	  (spawn-particle (particletype circle-hint-opaque)
					  x y 40 '((color . #x8b008ba0)
							   (r1 . 100.0)
							   (r2 . 0.0)))
	  (raylib:play-sound (sebundle-shortcharge sounds))
	  (wait 60)
	  (ease-to ease-in-out-quad x y 20 aiko)
	  (-> (cb)
		  (cbcount 16)
		  (cbspeed 3.0)
		  (cbshootenm aiko 'medium-ball-blue 2 (sebundle-release sounds)))
	  (-> (cb)
		  (cbcount 16)
		  (cbspeed 4.0)
		  (cbang 11.25)
		  (cbshootenm aiko 'medium-ball-cyan 2 #f))
	  (->
	   (spawn-bullet
		'big-star-red x y 2
		(λ (task blt)
		  (-> (cb)
			  (cbcount 8 15)
			  (cbspeed 1.8 4.0)
			  (cbabsolute-aim)
			  (cbang (centered-roll game-rng 180.0) (fl* winding 4.0))
			  (cbshoot x y
				(λ (layer in-layer speed facing)
				  (->
				   (spawn-bullet
					(vnth-mod colors in-layer)
					x y 10
					(λ (task blt)
					  (linear-step-decelerate facing speed -0.10 blt)
					  (wait-until (thunk (unbox spread-signal)))
					  (bullet-clrflags blt (bltflags uncancelable))
					  (wait (* 9 layer))
					  (linear-step-accelerate (fl+ facing pi) 0.0 0.02 4.0 blt)
					  (linear-step-forever (fl+ facing pi) 4.0 task blt)))
				   (bullet-addflags (bltflags uncancelable))))))
		  (wait-until (thunk (unbox spread-signal)))
		  (wait 30)
		  (cancel-bullet blt #t)))
	   (bullet-addflags (bltflags uncancelable)))
	  (wait 30)))
  (ease-to values +middle-boss-x+ +middle-boss-y+ 20 aiko)
  (raylib:play-sound (sebundle-longcharge sounds))
  (wait 20)
  (set-box! spread-signal #t)
  (let* ([move-task
		  (spawn-subtask "wiggle"
			(λ (task)
			  (dotimes 3
				(let ([x (fl+ (ex aiko) (centered-roll game-rng 50.0))]
					  [y (fl+ (ey aiko) (centered-roll game-rng 10.0))])
				  (ease-to ease-in-out-quad x y 30 aiko)
				  (wait 30))))
			task)]
		 [shoot-task
		  (spawn-subtask "shoot"
			(λ (task)
			  (interval-loop 30
				(dotimes 3
				  (-> (fb)
					  (fbcount 3)
					  (fbang 0.0 15.0)
					  (fbspeed 4.75)
					  (fbshootenm aiko 'heart-blue 5 (sebundle-shoot0 sounds)))
				  (wait 8))))
			move-task)])
	(wait-until (thunk (task-dead move-task)))))

(define (aiko-sp1 task aiko)
  (define bossinfo (enm-extras aiko))
  (define (keep-running)
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health aiko))))
  (set! current-chapter 21)
  (declare-spell aiko 5)
  (ease-to ease-in-out-quad +middle-boss-x+ +middle-boss-y+ 45 aiko)
  (wait 55)
  (loop-while
   (keep-running)
   (let ([t (spawn-subtask "wave"
			  (λ (task)
				(aiko-sp1-round task aiko)
				(wait 120))
			  task keep-running)])
	 (wait-until (thunk (task-dead t)))))
  (common-spell-postlude bossinfo aiko)
  (group-non2 task aiko))

(define (non2-familiar-on-death dead-signalbox enm)
  ;; invincible so on-death doesn't get spam-called, and we get rendered transparent
  (enm-addflags enm (enmflags invincible nocollide))
  (set-box! dead-signalbox #t)
  ;; same as standard logic but doesn't delete the enemy
  (spawn-enm-drops enm)
  (enemy-death-effects enm)
  #f)

(define non2-familiar-health 400)

(define (non2-dodo-control dead-signalbox task enm)
  (define (subtask)
	(spawn-subtask "sub"
	  (λ (task)
		(interval-loop 60
		  (-> (fb)
			  (fbcount 4 3)
			  (fbspeed 4.0 5.0)
			  (fbang 0.0 25.0)
			  (fbshootenm enm 'butterfly-red 5 (sebundle-shoot0 sounds)))
		  (-> (fb)
			  (fbcount 3 2)
			  (fbspeed 3.5 4.0)
			  (fbang 0.0 20.0)
			  (fbshootenm enm 'butterfly-magenta 5 #f))))
	  task
	  (thunk (not (unbox dead-signalbox)))))
  (enm-superarmor-set! enm 105)
  (wait 25)
  (raylib:play-sound (sebundle-opshow sounds))
  (ease-to ease-in-out-quad (ex enm) (fl+ 80.0 (ey enm)) 60 enm)
  (wait 20)
  (let loop ([st (subtask)])
	(wait-until (thunk (task-dead st)))
	(wait 180)
	(raylib:play-sound (sebundle-opshow sounds))
	(enm-health-set! enm non2-familiar-health)
	(enm-clrflags enm (enmflags invincible nocollide))
	(set-box! dead-signalbox #f)
	(loop (subtask))))

(define (non2-rere-control dead-signalbox task enm)
  (define (subtask)
	(spawn-subtask "sub"
	  (λ (task)
		(interval-loop 60
		  (-> (cb)
			  (cbcount 8)
			  (cbspeed 2.0)
			  (cbshootenm enm 'heart-orange 5 #f
						  (λ (facing speed task blt)
							(linear-step-decelerate facing speed -0.05 blt)
							(wait 30)
							(raylib:play-sound (sebundle-bell sounds))
							(linear-step-accelerate-forever
							 facing 0.0 0.08 3.0 task blt))))
		  (-> (cb)
			  (cbcount 16)
			  (cbspeed 2.0)
			  (cbshootenm
			   enm 'small-ball-yellow 5 #f
			   (λ (facing speed task blt)
				 (linear-step-decelerate facing speed -0.05 blt)
				 (wait 30)
				 (linear-step-accelerate-forever
				  (facing-player (bx blt) (by blt))
				  0.0 0.08 3.0 task blt))))))
	  task
	  (thunk (not (unbox dead-signalbox)))))
  (enm-superarmor-set! enm 80)
  (raylib:play-sound (sebundle-opshow sounds))
  (ease-to ease-in-out-quad (ex enm) (fl+ 80.0 (ey enm)) 60 enm)
  (wait 20)
  (let loop ([st (subtask)])
	(wait-until (thunk (task-dead st)))
	(wait 180)
	(raylib:play-sound (sebundle-opshow sounds))
	(enm-health-set! enm non2-familiar-health)
	(enm-clrflags enm (enmflags invincible nocollide))
	(set-box! dead-signalbox #f)
	(loop (subtask))))

(define (non2-mimi-control dead-signalbox task enm)
  (define (subtask first)
	(define delay 45)
	(define start-time frames)
	(define (stop-pred)
	  (or (unbox dead-signalbox)
		  (fx>= (fx- frames start-time) 150)))
	(define start-ang-to (facing-player (ex enm) (ey enm)))
	(define _ (raylib:play-sound (sebundle-laser sounds)))
	(define laser
	  (spawn-laser 'fixed-laser-blue
				   (ex enm) (ey enm)
				   start-ang-to
				   (fx2fl +playfield-height+)
				   5.0 20 delay
				   (λ (task blt)
					 (define ang-to (facing-player (bx blt) (by blt)))
					 (let ([turn-dir
							(if (fl< ang-to start-ang-to) -1.0 1.0)])
					   (interval-loop-while 15 (not (stop-pred))
						 (bullet-facing-set!
						  blt
						  (fl+ (bullet-facing blt) (fl* turn-dir (torad 4.0)))))))))
	(spawn-subtask "sub"
	  (λ (task)
		(wait delay)
		(interval-loop 30
		  (-> (fb)
			  (fbspeed 3.2)
			  (fbcount 20)
			  (fbabsolute-aim)
			  (fbang (todeg (fl+ (bullet-facing laser) pi)) 9.0)
			  (fbshootenm enm 'small-star-cyan 5 #f))))
	  task
	  (thunk (not (stop-pred)))))
  (enm-superarmor-set! enm 90)
  (wait 10)
  (raylib:play-sound (sebundle-opshow sounds))
  (ease-to ease-in-out-quad (ex enm) (fl+ 80.0 (ey enm)) 60 enm)
  (wait 20)
  (let loop ([st (subtask #t)])
	(wait-until (thunk (task-dead st)))
	;; If task exited from timeout and not from fairy kill, loop again
	(unless (unbox dead-signalbox)
	  (wait 60)
	  (loop (subtask #f)))
	(wait 180)
	(raylib:play-sound (sebundle-opshow sounds))
	(enm-health-set! enm non2-familiar-health)
	(enm-clrflags enm (enmflags invincible nocollide))
	(set-box! dead-signalbox #f)
	(loop (subtask #f))))

(define (group-non2 task aiko)
  (define bossinfo (blank-doremi-bossinfo))
  (define _ (wait 90))
  (define doremi
	(spawn-enemy (enmtype boss-doremi) 100.0 -100.0 500
				 (λ (task enm)
				   (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+
							60 enm))
				 '()
				 (constantly #f)))
  (define hazuki
	(spawn-enemy (enmtype boss-hazuki) -100.0 -100.0 500
				 (λ (task enm)
				   (ease-to ease-out-cubic +left-boss-x+ +left-boss-y+
							60 enm))
				 '()
				 (constantly #f)))
  (define (keep-running)
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi))))
  (set! current-chapter 22)
  (enm-extras-set! doremi bossinfo)
  (enm-extras-set! hazuki (blank-hazuki-bossinfo))
  (bossinfo-healthbars-set!
   bossinfo
   (adjust-bars-non (bossinfo-healthbars (enm-extras aiko))))
  (bossinfo-healthbars-set! (enm-extras aiko) '#())
  (enm-redirect-damage-set! hazuki doremi)
  (enm-redirect-damage-set! aiko doremi)
  (ease-to ease-out-cubic +right-boss-x+ +right-boss-y+ 60 aiko)
  (declare-nonspell doremi 1800 6000)
  (let* ([dodo-dead-signalbox (box #f)]
		 [dodo (-> (spawn-enemy
					'dodo
					(ex doremi) (ey doremi) non2-familiar-health
					(curry non2-dodo-control dodo-dead-signalbox)
					default-drop
					(curry non2-familiar-on-death dodo-dead-signalbox))
				   (enm-addflags (enmflags aura-red autocollect)))]
		 [rere-dead-signalbox (box #f)]
		 [rere (-> (spawn-enemy
					'rere
					(ex hazuki) (ey hazuki) non2-familiar-health
					(curry non2-rere-control rere-dead-signalbox)
					default-drop
					(curry non2-familiar-on-death rere-dead-signalbox))
				   (enm-addflags (enmflags aura-magenta autocollect)))]
		 [mimi-dead-signalbox (box #f)]
		 [mimi (-> (spawn-enemy
					'mimi
					(ex aiko) (ey aiko) non2-familiar-health
					(curry non2-mimi-control mimi-dead-signalbox)
					default-drop
					(curry non2-familiar-on-death mimi-dead-signalbox))
				   (enm-addflags (enmflags aura-blue autocollect)))])
	(wait-while keep-running))
  (common-nonspell-postlude bossinfo doremi)
  (group-sp2 task doremi hazuki aiko))

(define group-sp2-center-x 0.0)
(define group-sp2-center-y 224.0)
(define group-sp2-fairy-rotate-easing
  (bezier-cubic-easing 0.4 -0.3 0.6 1.3))

(define (group-sp2-fairy-wave-var1 this-ang task enm)
  (define is-dodo (eq? (enm-type enm) (enmtype dodo)))
  (spawn-particle (particletype circle-hint-opaque)
				  group-sp2-center-x group-sp2-center-y
				  90 '((color . #x8fff39)
					   (r1 . 35.0)
					   (r2 . 35.0)))
  (let ([orb #f])
	(do [(j 0 (add1 j))]
		[(= j 8)]
	  (raylib:play-sound (sebundle-shoot0 sounds))
	  (-> (fb)
		  (fbcount 3)
		  (fbabsolute-aim)
		  (fbang (fl+ (todeg this-ang) 180.0) 60.0)
		  (fbspeed 3.25)
		  (fbshoot (ex enm) (ey enm)
			(λ (row col speed facing)
			  (-> (spawn-bullet
				   (vnth '#(arrowhead-white
							arrowhead-red arrowhead-orange arrowhead-yellow
							arrowhead-green arrowhead-cyan
							arrowhead-blue arrowhead-magenta) j)
				   (ex enm) (ey enm) 5
				   (λ (task blt)
					 (linear-step-decelerate facing speed -0.04 blt)
					 (when (and is-dodo
								(not orb) (= col 1)) ;; first center blt to reach
					   (set! orb
							 (-> (spawn-bullet
								  'bubble-red
								  group-sp2-center-x group-sp2-center-y 5
								  values)
								 (bullet-addflags (bltflags uncancelable)))))
					 (wait 10)
					 (let ([nf (facing-point
								(bx blt) (by blt)
								group-sp2-center-x group-sp2-center-y)])
					   (bullet-facing-set! blt nf)
					   (loop-until (fl< (distsq (bx blt) (by blt)
												group-sp2-center-x group-sp2-center-y)
										100.0)
						 (linear-step nf speed blt)))
					 (delete-bullet blt)))
				  (bullet-addflags (bltflags uncancelable))
				  (bullet-facing-set! facing)))))
	  (wait 10))
	(raylib:play-sound (sebundle-longcharge sounds))
	(wait 145)
	(when is-dodo
	  (cancel-bullet orb #t))
	(wait 15)
	(raylib:play-sound (sebundle-oldvwoopfast sounds))
	(let loop ([j 0] [ang (centered-roll game-rng pi)])
	  (let ([type (vnth-mod
				   '#(arrowhead-red arrowhead-orange arrowhead-yellow
									arrowhead-green arrowhead-cyan
									arrowhead-blue arrowhead-magenta)
				   (quotient j 10))])
		(when is-dodo
		  (for-each
		   (λ (ang)
			 (define-values (x y)
			   (dist-away group-sp2-center-x group-sp2-center-y ang 12.0))
			 (spawn-bullet
			  type x y 5
			  (curry linear-step-forever ang 2.0)))
		   (list ang (fl+ ang hpi) (fl+ ang pi) (fl+ ang pi hpi))))
	    (yield))
	  (when (< j 110)
		(loop (add1 j)
			  (fl+ ang (torad (fl+ 21.0 (fx2fl j)))))))))

(define (group-sp2-fairy-wave-var2 ang task enm)
  (define is-dodo (eq? (enm-type enm) (enmtype dodo)))
  (define blts 64)
  (define gap-radius-blts 3)
  (define angper (torad (fl/ 360.0 (fx2fl blts))))
  (define init-ang (centered-roll game-rng pi))
  (define gap-winding (if (roll-bool game-rng) 4 -4))
  (let loop ([i 0] [gap 0]
			 ;; gross state. I had the j=0th bullet be responsible for the
			 ;; star at first, but if the first bullet gets gapped away
			 ;; then the star wouldn't be fired. instead just use a bit of
			 ;; state to only fire it once per ring
			 [ring-star-fired (box #f)])
	(when is-dodo
	  (raylib:play-sound (sebundle-shoot0 sounds))
	  (do [(j 0 (add1 j))]
		  [(= j blts)]
		(let*-values ([(ang) (fl+ init-ang (fl* angper (fx2fl j)))]
					  [(x y) (dist-away group-sp2-center-x group-sp2-center-y
										ang 155.0)])
		  (unless (or (<= (- gap gap-radius-blts) j (+ gap gap-radius-blts))
					  ;; also check against gap + blts for to account for
					  ;; boundary condition
					  (<= (- (+ gap blts) gap-radius-blts)
						  j (+ gap blts gap-radius-blts)))
			(-> (spawn-bullet (vnth-mod
							   '#(amulet-white
								  amulet-red amulet-orange amulet-yellow
								  amulet-green amulet-cyan
								  amulet-blue amulet-magenta) i)
							  x y 8
							  (λ (task blt)
								(loop-forever
								 (linear-step (fl+ ang pi) 2.25 blt)
								 (when (fl< (distsq (bx blt) (by blt)
													group-sp2-center-x
													group-sp2-center-y)
											25.0)
								   (delete-bullet blt)
								   (unless (unbox ring-star-fired)
									 (set-box! ring-star-fired #t)
									 (-> (fb)
										 (fbcount 1 3)
										 (fbspeed 2.5 3.5)
										 (fbang (if (positive? gap-winding)
													25.0 -25.0))
										 (fbshootez
										  (vnth-mod
										   '#(big-star-white
											  big-star-red big-star-orange big-star-yellow
											  big-star-green big-star-cyan
											  big-star-blue big-star-magenta) i)
										  group-sp2-center-x group-sp2-center-y
										  5 (sebundle-bell sounds))))))))
				(bullet-facing-set! (fl+ ang pi)))))))
	(wait 15)
	(when (< i 15)
	  (loop (add1 i) (mod (+ gap gap-winding) blts) (box #f)))))

(define (group-sp2-fairy-wave-var0 ang task enm)
  (define zone-radius 30.0)
  (define timeup-box (box #f))
  (when (eq? (enm-type enm) (enmtype dodo))
	(let ([p (spawn-particle (particletype circle-hint-opaque)
							 player-x player-y 300
							 `((r1 . ,zone-radius)
							   (r2 . ,zone-radius)
							   (color . #x7cfc0080)))])
	  (spawn-task "sync particle"
		(λ (task)
		  (loop-while (and (vector-index p live-particles)
						   (vector-index enm live-enm))
		   (particle-x-set! p player-x)
		   (particle-y-set! p player-y))
		  ;; clean up when the spell ends (i.e. fairy no longer in live-enm)
		  (delete-particle p))
		(constantly #t))))
  (do [(i 0 (add1 i))]
	  [(= i 8)]
	(-> (fb)
		(fbcount 12)
		(fbspeed 1.5)
		(fbabsolute-aim)
		(fbang (todeg (facing-point (ex enm) (ey enm)
									group-sp2-center-x group-sp2-center-y))
			   3.0)
		(fbshootenm
		 enm
		 (vnth
		  '#(arrowhead-white
			 arrowhead-red arrowhead-orange arrowhead-yellow
			 arrowhead-green arrowhead-cyan
			 arrowhead-blue arrowhead-magenta) i)
		 8 (sebundle-shoot0 sounds)
		 (λ (facing speed task blt)
		   (define hit-zone #f)
		   (let loop ()
			 (linear-step facing speed blt)
			 (when (fl< (distsq (bx blt) (by blt)
								player-x player-y)
						(fl* zone-radius zone-radius))
			   (raylib:play-sound (sebundle-bell sounds))
			   (set! hit-zone #t))
			 (unless (or (unbox timeup-box) hit-zone)
			   (yield)
			   (loop)))
		   (if hit-zone
			   (linear-step-forever
				(fl+ pi (facing-player (bx blt) (by blt)))
				speed task blt)
			   (begin
				 (bullet-facing-set!
				  blt (facing-player (bx blt) (by blt)))
				 (wait 30)
				 (linear-step-accelerate-forever
				  (bullet-facing blt)
				  0.0 0.08 4.0
				  task blt))))))
	(wait 30))
  (wait 60)
  (raylib:play-sound (sebundle-longcharge sounds))
  (set-box! timeup-box #t)
  (wait 60))

(define (group-sp2-fairy-ctrl init-ang task enm)
  (define init-dist 160.0)
  (define spin-time 120)
  (define-values (ix iy)
	(dist-away group-sp2-center-x group-sp2-center-y init-ang init-dist))
  (define _ (ease-to values ix iy 60 enm))
  (wait 60)
  (let wave ([wavei 0]
			 [ang init-ang])
	(do [(i 0 (add1 i))]
		[(fx= i spin-time)]
	  (let*-values
		  ([(dang) (lerp 0.0 (torad -120.0)
						 (group-sp2-fairy-rotate-easing
						  (inexact (/ i (sub1 spin-time)))))]
		   [(this-ang) (fl+ ang dang)]
		   [(x y)
			(dist-away group-sp2-center-x group-sp2-center-y this-ang init-dist)])
		(enm-x-set! enm x)
		(enm-y-set! enm y))
	  (yield))
	(let ([this-ang (fl- ang (torad 120.0))])
	  (case (mod wavei 3)
		[(0) (group-sp2-fairy-wave-var0 this-ang task enm)]
		[(1) (group-sp2-fairy-wave-var1 this-ang task enm)]
		[(2) (group-sp2-fairy-wave-var2 this-ang task enm)])
	  (wave (add1 wavei) this-ang))))

(define (group-sp2-boss-ctrl init-ang doremi enm task)
  (define dist 200.0)
  (define outer-spin-rate (torad 0.5))
  (define-values (ix iy)
	(dist-away group-sp2-center-x group-sp2-center-y init-ang dist))
  (enm-addflags enm (enmflags nocollide))
  (ease-to ease-in-out-quad ix iy 60 enm)
  (raylib:play-sound (sebundle-brasscharge sounds))
  (-> (spawn-enemy
	   (case (enm-type enm)
		 [(boss-doremi) (enmtype dodo)]
		 [(boss-hazuki) (enmtype rere)]
		 [(boss-aiko) (enmtype mimi)])
	   (ex enm) (ey enm) 1000 (curry group-sp2-fairy-ctrl init-ang))
	  (enm-addflags
	   (case (enm-type enm)
		 [(boss-doremi) (enmflags nocollide aura-red)]
		 [(boss-hazuki) (enmflags nocollide aura-magenta)]
		 [(boss-aiko)   (enmflags nocollide aura-blue)]))
	  (enm-redirect-damage-set! doremi))
  (spawn-particle
   (particletype circle-hint)
   group-sp2-center-x group-sp2-center-y
   120 `((easer . ,ease-out-quad) (color . -1) (r1 . 300.0) (r2 . 100.0)))
  (wait 60)
  (raylib:play-sound (sebundle-longcharge sounds))
  (wait 60)
  (raylib:play-sound (sebundle-release sounds))
  (let loop ([i 0]
			 [ang init-ang])
	(let-values ([(x y) (dist-away group-sp2-center-x group-sp2-center-y ang dist)])
	  (enm-x-set! enm x)
	  (enm-y-set! enm y))
	(let-values ([(q m) (div-and-mod i 5)])
	  (when (fxzero? m)
		(-> (spawn-bullet
			 'rice-white (ex enm) (ey enm) 5
			 (λ (task blt)
			   (linear-step-decelerate-to (fl+ ang pi) 2.0 -0.06 0.0 blt)
			   (wait 75)
			   (delete-bullet blt)
			   (parameterize ([ovr-nocanceldrop #t])
				 (-> (fb)
					 (fbcount 1 4)
					 (fbspeed 1.2 2.75)
					 (fbabsolute-aim)
					 (fbang (fl- (todeg ang) 20.0) -5.0)
					 (fbshootez
					  (vnth-mod '#(rice-red rice-orange rice-yellow
											rice-green rice-blue rice-magenta) q)
					  (bx blt) (by blt)
					  0 #f)))))
			(bullet-addflags (bltflags nocanceldrop))
			(bullet-facing-set! (fl+ ang pi)))))
	(yield)
	(loop (fx1+ i) (fl+ ang outer-spin-rate))))

(define (group-sp2 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (define (keep-running)
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi))))
  (set! current-chapter 23)
  (enm-redirect-damage-set! hazuki doremi)
  (enm-redirect-damage-set! aiko doremi)
  (declare-spell doremi 6)
  (wait 60)
  (raylib:play-sound (sebundle-shortcharge sounds))
  (spawn-subtask "aiko ctrl"
	(curry group-sp2-boss-ctrl (torad 30.0) doremi aiko)
	task keep-running)
  (spawn-subtask "hazuki ctrl"
	(curry group-sp2-boss-ctrl (torad 150.0) doremi hazuki)
	task keep-running)
  (spawn-subtask "doremi ctrl"
	(curry group-sp2-boss-ctrl (torad -90.0) doremi doremi)
	task keep-running)
  (wait-while keep-running)
  (common-spell-postlude bossinfo doremi)
  (doremi-non2 task doremi hazuki aiko))

;; TODO: clean this up?
(define (lbchevron enm type head-type
				   facing-deg dist layers max-spread stretch-per-layer
				   base-speed speed-factor)
  ;; fired from tail to tip
  (do [(i 0 (fx1+ i))]
	  [(fx= i layers)]
	(-> (lb)
		(lbang facing-deg)
		(lbdist (fl+ dist (fl* (fx2fl i) stretch-per-layer)))
		(lblen (lerp max-spread 0.0 (fl/ (fx2fl i) (fx2fl layers))))
		(lbcount 2)
		(lbspeed (fl+ base-speed (fl* (fx2fl (- layers i)) speed-factor)))
		(lbshootenm enm type 5 (sebundle-shootsoft sounds)))
	(let-values ([(x y) (dist-away (ex enm) (ey enm) (torad facing-deg)
								   (fl+ dist
										(fl* (fx2fl layers) stretch-per-layer)))])
	  (spawn-bullet head-type x y 5 (curry linear-step-forever
										   (torad facing-deg) base-speed)))))

(define (lbchev-ring enm type head-type init-ang max-spread)
  (define count 12)
  (define angper (fl/ 360.0 (fx2fl count)))
  (do [(i 0 (fx1+ i))]
	  [(fx= i count)]
	(lbchevron enm type head-type
			   (fl+ init-ang (fl* (fx2fl i) angper)) 30.0 3 max-spread 5.0 2.0 0.15)))

(define (doremi-non2 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (define (keep-running)
	(and (fxpositive? (bossinfo-remaining-timer bossinfo))
		 (fxpositive? (enm-health doremi))))
  (define (hurry)
	(fx<= (bossinfo-remaining-timer bossinfo) 600))
  (set! current-chapter 24)
  (wait 90)
  (adjust-bars-non (bossinfo-healthbars bossinfo))
  (spawn-subtask "hazuki leave"
	(λ (_)
	  (ease-to ease-out-cubic
			   (flcopysign 300.0 (ex hazuki))
			   (fl- (ey hazuki) 50.0) 60 hazuki)
	  (delete-enemy hazuki))
	task)
  (spawn-subtask "aiko leave"
	(λ (_)
	  (ease-to ease-out-cubic
			   (flcopysign 300.0 (ex aiko))
			   (fl- (ey aiko) 50.0) 60 aiko)
	  (delete-enemy aiko))
	task)
  (ease-to ease-in-out-quad +middle-boss-x+ +middle-boss-y+ 60 doremi)
  (enm-clrflags doremi (enmflags nocollide))
  (declare-nonspell doremi 2400 11000)
  (raylib:play-sound (sebundle-shortcharge sounds))
  (wait 60)
  (raylib:play-sound (sebundle-release sounds))
  (spawn-subtask "shoot"
	(λ (task)
	  (define init-ang (todeg (facing-player (ex doremi) (ey doremi))))
	  (let loop ([i 0])
		(let ([idx (mod (quotient i 3) 3)])
		  (lbchev-ring
		   doremi
		   (vnth-mod '#(amulet-magenta amulet-yellow amulet-blue) idx)
		   (vnth-mod '#(big-star-red big-star-orange big-star-cyan) idx)
		   (fl+ init-ang (fl* (fx2fl i) 13.0)) 40.0))
		(wait (if (hurry) 15 20))
		(loop (add1 i))))
	task keep-running)
  (spawn-subtask "side shots"
	(λ (task)
	  (define num 5)
	  (define bottom-y 430.0)
	  (define spacing 50)
	  (define top-y (fl- bottom-y (fx2fl (* spacing (sub1 num)))))
	  (define (get-y upwards i)
		(define space (fx2fl (* i spacing)))
		(if upwards (fl- bottom-y space) (fl+ top-y space)))
	  (define (ctrl type task blt)
		(wait 90)
		(cancel-bullet blt)
		(-> (fb)
			(fbcount 3 6)
			(fbspeed 4.0 6.0)
			(fbang 0.0 25.0)
			(fbshootez type (bx blt) (by blt) 2 (sebundle-bell sounds))))
	  (define large-types '#(glow-orb-red glow-orb-orange glow-orb-cyan))
	  (define small-types '#(music-red music-orange music-blue))
	  (wait 120)
	  (let loop ([j 0])
		(do [(i 0 (fx1+ i))]
			[(fx= i num)]
		  (let ([large-type (vnth-mod large-types j)]
				[y (get-y (fxeven? j) i)]
				[ctrl* (curry ctrl (vnth-mod small-types j))])
			(raylib:play-sound (sebundle-shoot0 sounds))
			(spawn-bullet large-type -160.0 y 5 ctrl*)
			(spawn-bullet large-type 160.0 y 5 ctrl*))
		  (wait 5))
		(wait 240)
		(loop (fx1+ j))))
	task keep-running)
  (spawn-subtask "hurry-sound"
	(λ (task)
	  (wait-until hurry)
	  (raylib:play-sound (sebundle-longcharge sounds)))
	task keep-running)
  (wait-while keep-running)
  (common-nonspell-postlude bossinfo doremi)
  (doremi-sp2 task doremi))

(define (steak-ctrl doremi steak task)
  (define speed 0.0)
  (wait 80)
  (loop-forever
   (let ([x (miscent-x steak)]
		 [y (miscent-y steak)])
	 ;; motion
	 (if (check-collision-circle-rec
		  player-x player-y +vacuum-radius-steak+
		  (- x 10) (- y 10) 20 20)
		 (let ([dir-to-player (v2unit (vec2 (- player-x x) (- player-y y)))])
		   (miscent-x-set! steak (+ x (* (v2x dir-to-player) 4)))
		   (miscent-y-set! steak (+ y (* (v2y dir-to-player) 4))))
		 (let ([dir-to-boss (v2unit (vec2 (- (ex doremi) x)
										  (- (ey doremi) y)))])
		   (miscent-x-set! steak (+ x (* (v2x dir-to-boss) speed)))
		   (miscent-y-set! steak (+ y (* (v2y dir-to-boss) speed)))
		   (when (fl< speed 4.0)
			 (set! speed (fl+ speed 0.05)))))
	 ;; collision
	 (cond
	  [(check-collision-circle-rec
		player-x player-y +hit-radius+ (- x 10) (- y 10) 20 20)
	   (raylib:play-sound (sebundle-dropbomb sounds))
	   (set-box! doremi-sp2-steaks-collected (1+ (unbox doremi-sp2-steaks-collected)))
	   (set! current-score (+ current-score item-value))
	   (spawn-particle
		(particletype itemvalue)
		x (- y 10.0)
		60 (cons #xffd70000 (number->string item-value)))
	   (delete-misc-ent steak)]
	  [(check-collision-circle-rec
		(ex doremi) (ey doremi) 8.0
		(- x 10) (- y 10) 20 20)
	   (raylib:play-sound (sebundle-ophide sounds))
	   (delete-misc-ent steak)]))))

(define (lbchev-ring2 enm type head-type init-ang)
  (define count 12)
  (define angper (fl/ 360.0 (fx2fl count)))
  (do [(i 0 (fx1+ i))]
	  [(fx= i count)]
	(lbchevron enm type head-type
			   (fl+ init-ang (fl* (fx2fl i) angper))
			   ;; dist layers max-spread stretch-per-layer base-speed speed-factor
			   30.0 3 45.0 -4.0 3.0 -0.12)))

(define (doremi-sp2 task doremi)
  (define bossinfo (enm-extras doremi))
  (define (keep-running)
	(and (fxpositive? (bossinfo-remaining-timer bossinfo))
		 (fxpositive? (enm-health doremi))))
  (define successes (box 0))
  (set! current-chapter 25)
  (declare-spell doremi 7)
  (set-box! doremi-sp2-steaks-collected 0)
  (ease-to ease-in-out-quad +middle-boss-x+ (fl+ 30.0 +middle-boss-y+) 60 doremi)
  (raylib:play-sound (sebundle-shortcharge sounds))
  (wait 30)
  (enm-addflags doremi (enmflags nocollide))
  (spawn-subtask "shoot"
	(λ (task)
	  (define wind 45)
	  (define rings-per-wave 6)
	  (interval-loop 250
		(dotimes 7
		  (-> (cb)
			  (cbcount 28)
			  (cbspeed 5.0)
			  (cbabsolute-aim)
			  (cbang (fl* 360.0 (roll game-rng)))
			  (cbshootez
			   'kunai-orange
			   (roll-flrange game-rng -50.0 50.0)
			   (roll-flrange game-rng (fl- (ey doremi) 30.0) (fl+ (ey doremi) 90.0))
			   5 (sebundle-shoot0 sounds)
			   (λ (facing speed task blt)
				 (define slow-speed 0.8)
				 (linear-step-decelerate-to facing speed -0.18 slow-speed blt)
				 (dotimes 70
				   (linear-step facing slow-speed blt)
				   (yield))
				 (raylib:play-sound (sebundle-bell sounds))
				 (linear-step-accelerate-forever
				  facing slow-speed 0.08 speed task blt))))
		  (wait 10))
		(wait 45)
		(raylib:play-sound (sebundle-shortcharge sounds))
		(wait 60)
		(raylib:play-sound (sebundle-release sounds))
		(spawn-subtask "spawn steaks"
		  ;; NB: Purposefully not bound so steak-ctrl uses outer task as parent
		  ;; Otherwise, the steak-ctrl task dies as soon as this task finishes
		  (λ (_) 
			(dotimes 5
			  (let* ([steak (spawn-misc-ent
							 'steak
							 (roll-sign
							  game-rng (roll-flrange game-rng 85.0 160.0))
							 (roll-flrange game-rng 80.0 110.0)
							 0.0 0.0)]
					 [steak-idx (vector-index steak live-misc-ents)])
				(raylib:play-sound (sebundle-opshow sounds))
				(spawn-subtask "steak control"
				  (curry steak-ctrl doremi steak)
				  task (thunk (eq? steak (vnth live-misc-ents steak-idx)))))
			  (wait 15)))
		  task)
		(do [(j 0 (add1 j))]
			[(= j rings-per-wave)]
		  (lbchev-ring2
		   doremi
		   (vnth-mod '#(amulet-magenta amulet-yellow amulet-blue) j)
		   (vnth-mod '#(big-star-red big-star-orange big-star-cyan) j)
		   (fx2fl (* j wind)))
		  (wait 12))))
	task keep-running)
  (wait-while keep-running)
  (common-spell-postlude
   bossinfo doremi
   (thunk
	(let ([count (unbox doremi-sp2-steaks-collected)])
	  (and (positive? count)
		   (* item-value (eround (expt 1.6 count)))))))
  (vector-for-each-truthy
   (λ (e) (when (eq? 'steak (miscent-type e)) (delete-misc-ent e)))
   live-misc-ents)
  (ease-to ease-in-out-quad +middle-boss-x+ +middle-boss-y+ 45 doremi)
  (hazuki-non2 task doremi))

(define (hazuki-non2-wisp-control ring1 ring2 task enm)
  (define start-frames frames)
  (spawn-subtask "control ring 1"
	(λ (task)
	  (loop-forever
	   (position-bullets-around
		(ex enm) (ey enm)
		(fl* 100.0 (flsin (fl* 1.0 (torad (fx2fl (- frames start-frames))))))
		(torad (fl/ (fx2fl (- frames start-frames)) 2.0))
		ring1)))
	task)
  (spawn-subtask "control ring 2"
	(λ (task)
	  (loop-forever
	   (position-bullets-around
		(ex enm) (ey enm)
		(fl* 60.0 (flsin (fl* 1.0 (torad (fx2fl (- frames start-frames))))))
		(torad (fl/ (fx2fl (- start-frames frames)) 2.0))
		ring2)))
	task)
  (when (not (flzero? (ex enm)))
	(spawn-subtask "anticheese"
	  (λ (task)
		(wait-until (thunk (fl> (ey enm) 350.0)))
		(interval-loop-until 15 (> (ey enm) +playfield-max-y+)
		  (when (or (and (flnegative? (ex enm)) (fl< player-x -100.0))
					(and (flpositive? (ex enm)) (fl> player-x 100.0)))
			(-> (fb)
				(fbcount 3 8)
				(fbang 0.0 5.0)
				(fbspeed 2.0 5.0)
				(fbshootenm enm 'knife-orange 2 (sebundle-bell sounds))))))
	  task))
  (ease-to values (ex enm) (fx2fl (+ +playfield-max-y+ 150)) 500 enm)
  (for-each delete-bullet ring1)
  (for-each delete-bullet ring2)
  (delete-enemy enm))

(define (hazuki-non2-spawn-one hazuki x)
  (define y 0.0)
  (define wisp-dead-box (box #f))
  (define (blt-ctrl task blt)
	(wait-until (thunk (unbox wisp-dead-box)))
	(let ([ex (car (unbox wisp-dead-box))]
		  [ey (cdr (unbox wisp-dead-box))])
	  (if (fl< (distsq (bx blt) (by blt) ex ey)
			   (fl* 30.0 30.0))
		  (cancel-bullet-with-drop blt (miscenttype big-piv) #t)
		  (cancel-bullet blt #t))))
  (define ring1
	(map
	 (λ (_)
	   (-> (spawn-bullet 'glow-ball-blue x y 5 blt-ctrl)
		   (bullet-addflags (bltflags uncancelable))))
	 (iota 8)))
  (define ring2
	(map
	 (λ (_)
	   (-> (spawn-bullet 'glow-ball-red x y 5 blt-ctrl)
		   (bullet-addflags (bltflags uncancelable))))
	 (iota 8)))
  (-> (spawn-enemy
	   (enmtype red-wisp) x y 350
	   (curry hazuki-non2-wisp-control ring1 ring2)
	   '()
	   (λ (enm)
		 (set-box! wisp-dead-box (cons (ex enm) (ey enm)))
		 (damage-enemy hazuki 400 #t #t)
		 #t))
	  (enm-addflags (enmflags aura-red nocollide))))

(define (hazuki-non2 task doremi)
  (define bars (bossinfo-healthbars (enm-extras doremi)))
  (define _ (wait 90))
  (define hazuki
	(spawn-enemy (enmtype boss-hazuki) 100.0 -100.0 500
				 (λ (task enm)
				   (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+
							60 enm))
				 '()
				 (constantly #f)))
  (define bossinfo (blank-hazuki-bossinfo))
  (define (keep-running)
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health hazuki))))
  (set! current-chapter 26)
  (spawn-subtask "doremi leave"
	(λ (_)
	  (ease-to ease-out-cubic -100.0 -100.0 60 doremi)
	  (delete-enemy doremi))
	task)
  (adjust-bars-non bars)
  (bossinfo-healthbars-set! bossinfo bars)
  (enm-extras-set! hazuki bossinfo)
  (declare-nonspell hazuki 2400 6500)
  (enm-superarmor-set! hazuki 2400)
  (wait 30)
  (spawn-subtask "main"
	(λ (task)
	  (raylib:play-sound (sebundle-oldvwoopslow sounds))
	  (wait 60)
	  (spawn-subtask "ring"
		(λ (task)
		  (wait 180)
		  (interval-loop 40
			(raylib:play-sound (sebundle-shoot0 sounds))
			(parameterize ([seal-distance 25.0]
						   [ovr-nocanceldrop #t])
			  (-> (cb)
				  (cbcount 56)
				  (cbabsolute-aim)
				  (cbspeed 3.0)
				  (cbshoot (ex hazuki) (ey hazuki)
					(λ (layer in-layer speed facing)
					  (spawn-bullet 'rice-white (ex hazuki) (ey hazuki) 5
									(curry linear-step-forever facing speed))))))))
		task)
	  (spawn-subtask "decor"
		(λ (task)
		  (wait 180)
		  (interval-loop 20
			(parameterize ([seal-distance 25.0]
						   [ovr-nocanceldrop #t])
			(-> (cb)
				(cbcount 56 2)
				(cbang 90.0)
				(cbabsolute-aim)
				(cbspeed 3.0 4.0)
				(cbshoot (ex hazuki) (ey hazuki)
				  (λ (layer in-layer speed facing)
					(unless (or (fx<= 0 in-layer 5)
								(fx<= 50 in-layer 55))
					  (spawn-bullet 'rice-yellow (ex hazuki) (ey hazuki) 5
									(curry linear-step-forever facing speed)))))))))
		task)
	  (interval-loop 90
		(hazuki-non2-spawn-one
		 hazuki
		 (vrand '#(-70.0 0.0 70.0) game-rng))))
	task keep-running)
  (wait-while keep-running)
  (common-nonspell-postlude bossinfo hazuki)
  (hazuki-sp2 task hazuki))

(define (hazuki-sp2-wisp-on-death toptask killed-by-hazuki-box hazuki enm)
  (define killed-by-hazuki (unbox killed-by-hazuki-box))
  (define (five-flower)
	(spawn-subtask "five-flower"
	  (λ (task)
		(define iters (if killed-by-hazuki 50 30))
		(define init-ang (fl* 360.0 (roll game-rng)))
		(define (fuzzed-player-x)
		  (fl+ player-x (centered-roll game-rng 3.0)))
		(define (fuzzed-player-y)
		  (fl+ player-y (centered-roll game-rng 3.0)))
		(define winding (if (roll-bool game-rng) 5 -5))
		(do [(i 0 (add1 i))]
			[(= i 30)]
		  (let ([facing1 (fl+ init-ang (fx2fl (* i winding)))]
				[facing2 (fl- init-ang (fx2fl (* i winding)))])
			(spawn-bullet
			 'butterfly-red
			 (ex enm) (ey enm) 5
			 (λ (task blt)
			   (linear-step-decelerate facing1 4.0 -0.10 blt)
			   (wait 5)
			   (raylib:play-sound (sebundle-bell sounds))
			   (linear-step-accelerate-forever
				(facing-point (bx blt) (by blt)
							  (fuzzed-player-x) (fuzzed-player-y))
				0.0 0.05 (if killed-by-hazuki 4.5 3.0) task blt)))
			(spawn-bullet
			 'butterfly-magenta
			 (ex enm) (ey enm) 5
			 (λ (task blt)
			   (linear-step-decelerate facing2 2.0 -0.05 blt)
			   (wait 5)
			   (raylib:play-sound (sebundle-bell sounds))
			   (linear-step-accelerate-forever
				(facing-point (bx blt) (by blt)
							  (fuzzed-player-x) (fuzzed-player-y))
				0.0 0.05 (if killed-by-hazuki 4.0 2.5) task blt)))
			(yield))))
	  toptask))
  (define (expanding-ring)
	(define winding (if (roll-bool game-rng) (torad 2.2) (torad -2.2)))
	(-> (cb)
		(cbabsolute-aim)
		(cbang (fl* 360.0 (roll game-rng)))
		(cbcount 24 (if killed-by-hazuki 3 2))
		(cbspeed 1.8 3.5)
		(cbshoot (ex enm) (ey enm)
		  (λ (layer in-layer speed facing)
			(-> (spawn-bullet
				 (vnth '#(butterfly-orange butterfly-magenta butterfly-red) layer)
				 (ex enm) (ey enm) 5
				 (λ (task blt)
				   (let loop ([facing facing]
							  [i 0])
					 (bullet-facing-set! blt facing)
					 (if (< i 60)
						 (begin
						   (linear-step facing 1.5 blt)
						   (yield)
						   (loop (fl+ facing winding)
								 (add1 i)))
						 (begin
						   (raylib:play-sound (sebundle-bell sounds))
						   (linear-step-accelerate-forever
							facing 1.5
							0.04 speed task blt))))))
				(bullet-facing-set! facing))))))
  (define (chevron)
	(define layers (if killed-by-hazuki 8 5))
	(define speed (if killed-by-hazuki 4.0 3.0))
	(define (do-chevron type facing)
	  (do [(i 0 (add1 i))]
		  [(= i layers)]
		(-> (fb)
			(fbcount (if (zero? i) 1 2))
			(fbabsolute-aim)
			(fbang (todeg facing) (if (zero? i) 0.0 (fx2fl (* i 4))))
			(fbspeed speed)
			(fbshootenm enm type (* i 4) #f))))
	(define base-facing (facing-player (ex enm) (ey enm)))
	(do-chevron 'butterfly-red base-facing)
	(do-chevron 'butterfly-blue (fl+ base-facing (torad 40.0)))
	(do-chevron 'butterfly-orange (fl+ base-facing (torad -40.0))))
  ;; don't run when being cleared by the attack ending
  (when (fxpositive? (enm-health hazuki))
	(case (roll game-rng 3)
	  [(0) (five-flower)]
	  [(1) (expanding-ring)]
	  [(2) (chevron)])
	(unless killed-by-hazuki
	  (damage-enemy hazuki 800 #t #t))))

(define (hazuki-sp2-wave all-waves-clean-box toptask hazuki)
  (define points (vector-shuffle '#((135.0 . 145.0) (-135.0 . 145.0)
									(77.0 . 225.0) (-77.0 . 225.0))
								 game-rng))
  (define start-time frames)
  (define enms-and-boxes
	(let loop ([acc '()]
			   [i 0])
	  (if (= i 4)
		  (reverse! acc)
		  (let ([killed-by-hazuki-box (box #f)]
				[x (car (vnth points i))]
				[y (cdr (vnth points i))])
			(wait 20)
			(raylib:play-sound (sebundle-brasscharge sounds))
			(loop 
			 (cons
			  (cons (-> (spawn-enemy
						 (enmtype red-wisp) (ex hazuki) (ey hazuki) 350
						 (λ (task enm)
						   (enm-superarmor-set! enm 40)
						   (ease-to ease-in-out-quad x y 45 enm)
						   (loop-forever))
						 '() (curry hazuki-sp2-wisp-on-death
									toptask killed-by-hazuki-box hazuki))
						(enm-addflags (enmflags aura-red)))
					killed-by-hazuki-box)
			  acc)
			 (add1 i))))))
  (define (all-dead)
	(for-all (λ (pair) (fxnonpositive? (enm-health (car pair))))
			 enms-and-boxes))
  (define killer-task
	(spawn-subtask "kill"
	  (λ (task)
		(interval-loop-until 25 (fx>= (fx- frames start-time) 200)
		  (-> (fb)
			  (fbcount 8)
			  (fbspeed 2.0)
			  (fbang 0.0 20.0)
			  (fbshootenm hazuki 'rest-blue 5 (sebundle-shoot0 sounds))))
		(for-each
		 (λ (pair)
		   (define e (car pair))
		   (wait 20)
		   (when (fxpositive? (enm-health e))
			 (raylib:play-sound (sebundle-longcharge sounds))
			 (spawn-particle (particletype circle-hint-opaque)
							 (ex e) (ey e) 30
							 '((color . #x8b008ba0)
							   (r1 . 100.0)
							   (r2 . 20.0)))))
		 enms-and-boxes)
		(raylib:play-sound (sebundle-oldvwoopfast sounds))
		(for-each
		 (λ (pair)
		   (define e (car pair))
		   (define killed-by-hazuki-box (cdr pair))
		   (define facing
			 (facing-point (ex hazuki) (ey hazuki)
						   (ex e) (ey e)))
		   (when (fxpositive? (enm-health e))
			 (wait 20)
			 (spawn-bullet
				  'glow-orb-red (ex hazuki) (ey hazuki) 5
				  (λ (task blt)
					(define start-time frames)
					(spawn-subtask "trail"
					  (λ (task)
						(loop-forever
						 (spawn-bullet
						  'small-ball-magenta (bx blt) (by blt) 8
						  (λ (task blt)
							(define facing
							  (torad (fx2fl
									  (fx* 24 (fx- frames start-time)))))
							(linear-step-accelerate-forever
							 facing
							 0.25 0.02 4.0 task blt)))))
					  task)
					(loop-forever
					  (linear-step facing 6.0 blt)
					  (let-values ([(x y w h) (enm-hurtbox e)])
						(when (and
							   (fxpositive? (enm-health e))
							   (check-collision-circle-rec
								(bx blt) (by blt)
								(bullet-hit-radius (bullet-type blt))
								x y w h))
						  (spawn-subtask "pellets"
							(λ (task)
							  (define wind-dir (if (flnegative? x) -1 1))
							  (define init-facing (fl* 360.0 (roll game-rng)))
							  (do [(i 0 (add1 i))]
								  [(= i 40)]
								(spawn-bullet
								 'pellet-blue
								 (fl+ x (fl/ w 2.0)) (fl+ y (fl/ h 2.0)) 5
								 (λ (task blt)
								   (define facing
									 (torad (fl+ init-facing (fx2fl (* 2 i
																	   wind-dir
																	   13)))))
								   (define speed (roll-flrange
												  game-rng 2.0 2.5))
								   (linear-step-forever facing 2.0 task blt)))
								(spawn-bullet
								 'pellet-blue
								 (fl+ x (fl/ w 2.0)) (fl+ y (fl/ h 2.0)) 5
								 (λ (task blt)
								   (define facing
									 (torad (fl+ init-facing (fx2fl (* (add1 (* 2 i))
																	   wind-dir
																	   13)))))
								   (define speed (roll-flrange
												  game-rng 2.0 2.5))
								   (linear-step-forever facing 2.0 task blt)))
								(yield)))
							toptask)
						  (set-box! all-waves-clean-box #f)
						  (set-box! killed-by-hazuki-box #t)
						  (kill-enemy e)
						  (cancel-bullet blt))))))))
		 enms-and-boxes))
	  toptask
	  (thunk (not (all-dead)))))
  (wait-until (thunk (task-dead killer-task))))

(define (hazuki-sp2 task hazuki)
  (define bossinfo (enm-extras hazuki))
  (define (keep-running)
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health hazuki))))
  (define all-waves-clean (box #t))
  (set! current-chapter 27)
  (declare-spell hazuki 8)
  (enm-superarmor-set! hazuki (bossinfo-remaining-timer bossinfo))
  (ease-to ease-out-quad 0.0 179.0 45 hazuki)
  (wait 45)
  (spawn-subtask "main"
	(λ (task)
	  (interval-loop 120
		(hazuki-sp2-wave all-waves-clean task hazuki)))
	task keep-running)
  (wait-while keep-running)
  (common-spell-postlude
   bossinfo hazuki
   ;; Extra bonus if all waves were cleared without a wisp being killed by hazuki
   (thunk (and (unbox all-waves-clean) (* 100 item-value))))
  (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+ 45 hazuki)
  (aiko-non2 task hazuki))

(define (aiko-non2-laser-ctrl is-right aiko task blt)
  (define start-ang -hpi)
  (define dang (torad 160.0))
  (do [(i 0 (fx1+ i))]
	  [(fx= i 300)]
	(let-values ([(x) (ex aiko)]
				 [(y) (ey aiko)]
				 [(facing)
				  (if is-right
					  (fl+ start-ang (inexact (lerp 0 dang (ease-out-quad (/ i 300)))))
					  (fl- start-ang
						   (inexact (lerp 0 dang (ease-out-quad (/ i 300))))))]
				 [(ring-wave ring-rem) (div-and-mod i 15)])
	  (bullet-x-set! blt x)
	  (bullet-y-set! blt y)
	  (bullet-facing-set! blt facing)
	  (when (and (fxzero? ring-rem) (fx<= i 180))
		(for-each
		 (λ (x y)
		   (when (and x y)
			 (-> (cb)
				 (cbcount 20 3)
				 (cbspeed 2.0 4.0)
				 (cbshoot x y
				   (λ (layer in-layer speed facing)
					 (define type (vnth '#(small-star-orange
										   small-star-green
										   small-star-cyan)
										layer))
					 (-> (spawn-bullet
						  type x y 10 (curry linear-step-forever facing speed))
						 (bullet-facing-set! facing)))))
			 (raylib:play-sound (sebundle-bell sounds))))
		 (list (hit-top-x x y facing)
			   (hit-bot-x x y facing)
			   (fx2fl +playfield-min-x+)
			   (fx2fl +playfield-max-x+))
		 (list (fx2fl +playfield-min-y+)
			   (fx2fl +playfield-max-y+)
			   (hit-left-y x y facing)
			   (hit-right-y x y facing)))))
	(yield))
  (loop-forever))

(define (aiko-non2 task hazuki)
  (define bars (bossinfo-healthbars (enm-extras hazuki)))
  (define _ (wait 90))
  (define aiko
	(spawn-enemy (enmtype boss-aiko) 100.0 -100.0 500
				 (λ (task enm)
				   (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+
							60 enm))
				 '()
				 (constantly #f)))
  (define bossinfo (blank-aiko-bossinfo))
  (define (keep-running)
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health aiko))))
  (set! current-chapter 28)
  (spawn-subtask "hazuki leave"
	(λ (_)
	  (ease-to ease-out-cubic -100.0 -100.0 60 hazuki)
	  (delete-enemy hazuki))
	task)
  (adjust-bars-non bars)
  (bossinfo-healthbars-set! bossinfo bars)
  (enm-extras-set! aiko bossinfo)
  (declare-nonspell aiko 1800 6500)
  (wait 90)
  (raylib:play-sound (sebundle-laser sounds))
  (parameterize ([ovr-uncancelable #t])
	(for-each
	 (λ (is-right)
	   (spawn-laser 'fixed-laser-blue
					(ex aiko) (ey aiko)
					-hpi
					(fx2fl +playfield-height+)
					5.0
					40 60 (curry aiko-non2-laser-ctrl is-right aiko)))
	 '(#f #t)))
  (wait 60)
  (spawn-subtask "rage rings"
	(λ (task)
	  (interval-loop 23
		(when (or (fx<= (bossinfo-remaining-timer bossinfo) 900)
				  (fl<= player-y (fl+ (ey aiko) 100.0)))
		  (-> (cb)
			  (cbcount 48)
			  (cbspeed 4.0)
			  (cbang (if (roll-bool game-rng) 0.0 2.0))
			  (cbshootenm aiko 'kunai-red 2 (sebundle-shoot0 sounds))))))
	task keep-running)
  (spawn-subtask "rings"
	(λ (task)
	  (interval-loop-while 60 (fx<= (bossinfo-elapsed-frames bossinfo) 410)
		(-> (cb)
			(cbcount 12)
			(cbspeed 3.0)
			(cbshootenm aiko 'heart-blue 2 (sebundle-shoot0 sounds))))
	  (raylib:play-sound (sebundle-longcharge sounds))
	  (spawn-subtask "decor"
		(λ (task)
		  (interval-loop 23
			(-> (fb)
				(fbcount 15)
				(fbspeed 4.0)
				(fbabsolute-aim)
				(fbang -90.0 12.0)
				(fbshootenm aiko 'big-star-orange 2 (sebundle-bell sounds)))))
		task keep-running)
	  (interval-loop 46
		(-> (fb)
			(fbcount 7)
			(fbspeed 3.0)
			(fbang 0.0 8.0)
			(fbshootenm aiko 'heart-blue 2 (sebundle-shoot0 sounds)))))
	task keep-running)
  (ease-to values +middle-boss-x+ 225.0 300 aiko)
  (wait-while keep-running)
  (common-nonspell-postlude bossinfo aiko #t)
  (aiko-sp2 task aiko))

(define aiko-sp2-x-margin 24)
(define aiko-sp2-x-left (fx2fl (+ +playfield-min-x+ aiko-sp2-x-margin)))
(define aiko-sp2-x-right (fx2fl (- +playfield-max-x+ aiko-sp2-x-margin)))
(define aiko-sp2-x-len (fl- aiko-sp2-x-right aiko-sp2-x-left))
(define aiko-sp2-y-margin-top 34)
(define aiko-sp2-y-margin-bot 23)
(define aiko-sp2-y-top (fx2fl aiko-sp2-y-margin-top))
(define aiko-sp2-y-bot (fx2fl (- +playfield-max-y+ aiko-sp2-y-margin-bot)))
(define aiko-sp2-y-len (fl- aiko-sp2-y-bot aiko-sp2-y-top))
(define aiko-sp2-goal-left -67.0)
(define aiko-sp2-goal-right (fl- aiko-sp2-goal-left))
(define aiko-sp2-laser-radius 5.0)
(define aiko-sp2-bot-len (fl- aiko-sp2-x-right aiko-sp2-goal-right))

(define aiko-sp2-vertical-rects
  (list
   ;; left
   (make-rectangle
	;; extra buffer for high speed collisions lmao
	(fl- aiko-sp2-x-left aiko-sp2-laser-radius 40.0)
	aiko-sp2-y-top
	(fl+ (fl* 2.0 aiko-sp2-laser-radius) 40.0)
	aiko-sp2-y-len)
   ;; right
   (make-rectangle
	(fl- aiko-sp2-x-right aiko-sp2-laser-radius)
	aiko-sp2-y-top
	(fl+ (fl* 2.0 aiko-sp2-laser-radius) 40.0)
	aiko-sp2-y-len)
   ;; goal left
   (make-rectangle
	(fl- aiko-sp2-goal-left aiko-sp2-laser-radius)
	(fl- aiko-sp2-y-bot aiko-sp2-laser-radius)
	(fl* 2.0 aiko-sp2-laser-radius)
	(fx2fl (fx+ aiko-sp2-y-margin-bot 10)))
   ;; goal right
   (make-rectangle
	(fl- aiko-sp2-goal-right aiko-sp2-laser-radius)
	(fl- aiko-sp2-y-bot aiko-sp2-laser-radius)
	(fl* 2.0 aiko-sp2-laser-radius)
	(fx2fl (fx+ aiko-sp2-y-margin-bot 10)))))
(define aiko-sp2-horiz-rects
  (list
   ;; top
   (make-rectangle
	aiko-sp2-x-left
	(fl- aiko-sp2-y-top aiko-sp2-laser-radius)
	aiko-sp2-x-len
	(fl* 2.0 aiko-sp2-laser-radius))
   ;; bottom left
   (make-rectangle
	aiko-sp2-x-left
	(fl- aiko-sp2-y-bot aiko-sp2-laser-radius)
	(fl- aiko-sp2-bot-len aiko-sp2-laser-radius)
	(fl* 2.0 aiko-sp2-laser-radius))
   ;; bottom right
   (make-rectangle
	(fl- aiko-sp2-goal-right aiko-sp2-laser-radius)
	(fl- aiko-sp2-y-bot aiko-sp2-laser-radius)
	(fl+ aiko-sp2-bot-len (fl* 2.0 aiko-sp2-laser-radius))
	(fl* 2.0 aiko-sp2-laser-radius))))
(define aiko-sp2-rects (append aiko-sp2-vertical-rects aiko-sp2-horiz-rects))


(define (aiko-sp2-ball-ctrl msg-box ball-killing-blow-box aiko task blt)
  (let loop ([state 'stop]
			 [facing 0.0]
			 [speed 0.0]
			 [frames-moving 0])
	(let ([msg (unbox msg-box)])
	  (when msg
		(set-box! msg-box #f)
		(record-case msg
		  [(stop) ()
		   (loop 'stop 0.0 0.0 0)]
		  [(attach) ()
		   (loop 'attach 0.0 0.0 0)]
		  [(move) (facing speed)
		   (loop 'move facing speed 0)])))

	(case state
	  [(stop)
	   (unless aiko
		 (cancel-bullet blt #t))
	   (yield)
	   (loop state facing speed 0)]
	  [(attach)
	   (bullet-x-set! blt (ex aiko))
	   (bullet-y-set! blt (ey aiko))
	   (yield)
	   (loop state facing speed 0)]
	  [(move)
	   (when (flnegative? speed)
		 (loop 'stop 0.0 0.0 0))
	   (let ([ox (bx blt)]
			 [oy (by blt)])
		 (linear-step facing speed blt)
		 (yield)

		 (when (> (by blt) (+ +playfield-max-y+ 10))
		   (damage-player)
		   (cancel-bullet blt #t)
		   ;; hacky imperative return to stop doing further processing
		   ;; we won't resume anymore because we canceled the bullet
		   (yield))

		 ;; collide with player
		 (let ([dx (fl- (bx blt) player-x)]
			   [dy (fl- (by blt) player-y)]
			   [maxdist (fl+ (bullet-hit-radius (bullet-type blt))
							 +graze-radius+)])
		   ;; NB: standard intersection test, not the orthogonal one we use for
		   ;; bullets in the rest of the game.
		   (when (fl<= (fl+ (fl* dx dx) (fl* dy dy))
					   (fl* maxdist maxdist))
			 ;; just push the ball away and give a small speed boost
			 (loop state
				   (if (and (flzero? (v2x last-player-movement-dir))
							(flzero? (v2y last-player-movement-dir)))
					   (facing-point player-x player-y
									 (bx blt) (by blt))
					   (flatan (v2y last-player-movement-dir)
							   (v2x last-player-movement-dir)))
				   (fl* speed 1.01)
				   (fx1+ frames-moving))))

		 ;; collide with aiko
		 (when (fx> frames-moving 10) ;; prevent damaging on kick
		   (let ([dx (fl- (bx blt) (ex aiko))]
				 [dy (fl- (by blt) (ey aiko))]
				 [maxdist (fl+ (bullet-hit-radius (bullet-type blt))
							   30.0)])
			 (when (fl<= (fl+ (fl* dx dx) (fl* dy dy))
						 (fl* maxdist maxdist))
			   (let ([old-health (enm-health aiko)])
				 (damage-enemy aiko 2800)
				 (when (and (fxpositive? old-health)
							(fxnonpositive? (enm-health aiko)))
				   (set-box! ball-killing-blow-box #t)))
			   (loop state
					 (facing-point (ex aiko) (ey aiko)
								   (bx blt) (by blt))
					 (fl* speed 1.01)
					 (fx1+ frames-moving)))))

		 ;; bounce off walls
		 (let*-values ([(new-pos new-facing)
						(do-bounce-off (vec2 (bx blt) (by blt))
									   (bullet-hit-radius (bullet-type blt))
									   facing aiko-sp2-rects)]
					   [(nx) (v2x new-pos)]
					   [(ny) (v2y new-pos)])
		   (bullet-x-set! blt nx)
		   (bullet-y-set! blt ny)
		   (when (not (epsilon-equal new-facing facing))
			 (let ([consume (λ (low layer in-layer speed facing)
							  (raylib:play-sound (sebundle-bell sounds))
							  (spawn-bullet
							   (if low
								   (if (zero? layer)
									   'small-star-white 'small-star-yellow)
								   (if (zero? layer)
									   'small-star-orange 'small-star-red))
							   nx ny 5
							   (curry linear-step-forever facing speed)))])
			   (if (fl> ny 288.0)
				   (-> (cb)
					   (cbcount 12 2)
					   (cbspeed 2.0 4.0)
					   (cbang 15.0)
					   (cbshoot nx ny (curry consume #t)))
				   (-> (cb)
					   (cbcount 24 2)
					   (cbspeed 3.0 5.0)
					   (cbabsolute-aim)
					   (cbang (fx2fl (roll game-rng 360)))
					   (cbshoot nx ny (curry consume #f))))))
		   (loop state new-facing (fl- speed 0.01) (fx1+ frames-moving))))])))

(define (aiko-sp2 task aiko)
  (define bossinfo (enm-extras aiko))
  (define (keep-running)
	(and (fxpositive? (bossinfo-remaining-timer bossinfo))
		 (fxpositive? (enm-health aiko))))
  (define ball-killing-blow-box (box #f))
  (set! current-chapter 29)
  (declare-spell aiko 9)
  ;; this is just for debug jumps, the previous nonspell should already leave
  ;; us here
  (enm-x-set! aiko +middle-boss-x+)
  (enm-y-set! aiko 225.0)
  ;; also just for debug jumps to show the right size
  (let ([bars (bossinfo-healthbars bossinfo)])
	(healthbar-width-set! (vnth bars (sub1 (vlen bars))) 25))
  (wait 120)
  (raylib:play-sound (sebundle-brasscharge sounds))
  (parameterize ([ovr-uncancelable #t])
	(let ([ctrl (λ (_task _blt) (loop-forever))])
	  (for-each
	   (λ (l) (bullet-addflags l (bltflags noshine)))
	   (list
		;; top edge
		(spawn-laser 'fixed-laser-blue
					 aiko-sp2-x-left aiko-sp2-y-top
					 0.0 aiko-sp2-x-len aiko-sp2-laser-radius
					 40 60 ctrl)
		;; left edge
		(spawn-laser 'fixed-laser-blue aiko-sp2-x-left aiko-sp2-y-top
					 hpi aiko-sp2-y-len aiko-sp2-laser-radius
					 40 60 ctrl)
		;; right edge
		(spawn-laser 'fixed-laser-blue aiko-sp2-x-right aiko-sp2-y-top
					 hpi aiko-sp2-y-len aiko-sp2-laser-radius
					 40 60 ctrl)))
	  ;; corners
	  (spawn-bullet 'big-star-blue aiko-sp2-x-left aiko-sp2-y-top 15 values)
	  (spawn-bullet 'big-star-blue aiko-sp2-x-right aiko-sp2-y-top 15 values)
	  (spawn-bullet 'big-star-blue aiko-sp2-x-left aiko-sp2-y-bot 15 values)
	  (spawn-bullet 'big-star-blue aiko-sp2-x-right aiko-sp2-y-bot 15 values)
	  ;; bottom
	  (spawn-laser 'fixed-laser-blue aiko-sp2-goal-left aiko-sp2-y-bot
				   pi aiko-sp2-bot-len aiko-sp2-laser-radius
				   40 60 ctrl)
	  (spawn-laser 'fixed-laser-blue aiko-sp2-goal-left aiko-sp2-y-bot
				   hpi 50.0 aiko-sp2-laser-radius
				   40 60 ctrl)
	  (spawn-laser 'fixed-laser-blue aiko-sp2-goal-right aiko-sp2-y-bot
				   0.0 aiko-sp2-bot-len aiko-sp2-laser-radius
				   40 60 ctrl)
	  (spawn-laser 'fixed-laser-blue aiko-sp2-goal-right aiko-sp2-y-bot
				   hpi 50.0 aiko-sp2-laser-radius
				   40 60 ctrl)
	  ;; penalty box (ball won't bounce off these lasers)
	  ;; top
	  (-> (spawn-laser 'fixed-laser-cyan -85.0 (fl- aiko-sp2-y-bot 90.0)
					   0.0 160.0 3.0
					   40 60 ctrl)
		  (bullet-addflags (bltflags noshine)))
	  ;; left
	  (spawn-laser 'fixed-laser-cyan -85.0 (fl- aiko-sp2-y-bot 90.0)
				   hpi 85.0 3.0
				   40 60 ctrl)
	  ;; right
	  (spawn-laser 'fixed-laser-cyan 85.0 (fl- aiko-sp2-y-bot 90.0)
				   hpi 85.0 3.0
				   40 60 ctrl)))
  (spawn-particle
   (particletype text-hint) 0.0 280.0 150
   `((color . ,aiko-color)
	 (text . "Don't let Aiko score a goal!")
	 (size . 24.0)))
  (wait 60)
  (raylib:play-sound (sebundle-laser sounds))
  (spawn-subtask "protection"
	(λ (task)
	  (interval-loop 20
		(when (fl< player-y 330.0)
		  (-> (cb)
			  (cbcount 36)
			  (cbspeed 2.0)
			  (cbshootenm aiko 'kunai-magenta 5 #f)))))
	task keep-running)
  (spawn-subtask "main"
	(λ (task)
	  (define (spawn-ball)
		(parameterize ([ovr-uncancelable #t]
					   [ovr-noclip #t])
		  (spawn-bullet 'yinyang-green 0.0 200.0 5
						(curry aiko-sp2-ball-ctrl
							   msg-box ball-killing-blow-box aiko))))
	  (define msg-box (box #f))
	  (define cur-ball (spawn-ball))
	  (ease-to values 0.0 190.0 60 aiko)
	  (wait 60)
	  ;; at the beginning of each wave, aiko and ball are both assumed to be
	  ;; in their proper initial y: ball at 200, aiko at 190
	  (let wave ([start-frames frames])
		(raylib:play-sound (sebundle-longcharge sounds))
		(ease-to ease-in-out-quart
				 (vnth '#(-20.0 0.0 20.0) (roll game-rng 3))
				 160.0 30 aiko)
		(wait 10)
		(ease-to values 0.0 190.0 10 aiko)
		(-> (fb)
			(fbcount 3 3)
			(fbang 0.0 10.0)
			(fbspeed 3.0 4.0)
			(fbshootenm aiko 'small-ball-white 5 #f))
		(raylib:play-sound (sebundle-shoot0 sounds))
		(let* ([try-bounce-shot
				(and (fl< player-y 380.0)
					 (or (fl<= -28.0 player-x 28.0)
						 (fxzero? (roll game-rng 3))))]
			   [target-x
				(if try-bounce-shot
					(cond
					 [(fl>= player-x 28.0) -161.0]
					 [(fl<= player-x -28.0) 161.0]
					 [else (if (roll-bool game-rng) -161.0 161.0)])
					(cond
					 [(fl>= player-x 28.0)
					  (fx2fl (- (roll game-rng 20)))]
					 [(fl<= player-x -28.0)
					  (fx2fl (roll game-rng 20))]
					 [else (if (roll-bool game-rng) -38.0 38.0)]))]
			   [target-y (if try-bounce-shot 330.0 (fx2fl +playfield-max-y+))]
			   [ball-facing
				(facing-point (bx cur-ball) (by cur-ball)
							  target-x target-y)])
		  (set-box! msg-box (list 'move ball-facing 9.0)))
		(wait-until (thunk (or (fx> (fx- frames start-frames) 480)
							   (not (vector-index cur-ball live-bullets)))))
		(if (vector-index cur-ball live-bullets)
			(begin
			  (set-box! msg-box '(stop))
			  (enm-addflags aiko (enmflags nocollide))
			  (ease-to ease-in-out-quad
					   (bx cur-ball) (by cur-ball) 60 aiko)
			  (set-box! msg-box '(attach))
			  (ease-to ease-in-out-quad 0.0 190.0 60 aiko)
			  (set-box! msg-box '(stop))
			  (enm-clrflags aiko (enmflags nocollide))
			  (wait 10)
			  (wave frames))
			(begin
			  (ease-to ease-in-out-quad 0.0 190.0 60 aiko)
			  (set! cur-ball (spawn-ball))
			  (wait 60)
			  (wave frames)))))
	task keep-running)
  (wait-while keep-running)
  (common-spell-postlude
   bossinfo aiko
   (thunk (and (unbox ball-killing-blow-box) (* 100 item-value))))
  (group-sp3 task aiko))

(define group-sp3-right-x 150.0)
(define group-sp3-left-x -150.0)
(define group-sp3-top-y 85.0)
(define group-sp3-bot-y 410.0)
(define (group-sp3-setup-field task)
  (define ringrad 30.0)
  (define ringcnt 24)
  (define (ring cx cy start-i end-i)
	(define angper (fl/ tau (fx2fl ringcnt)))
	(define (body i)
	  (let*-values ([(ang) (fl* angper (fx2fl i))]
					[(x y) (dist-away cx cy ang ringrad)])
		(raylib:play-sound (sebundle-shootsoft sounds))
		(-> (spawn-bullet 'pellet-blue x y 5 values)
			(bullet-addflags (bltflags uncancelable)))
		(wait 4)))
	(if (< start-i end-i)
		(do [(i start-i (add1 i))]
			[(> i end-i)]
		  (body i))
		(do [(i start-i (sub1 i))]
			[(< i end-i)]
		  (body i))))
  (define (lace-horiz points)
	(do [(t 0.0 (fl+ t 0.05))]
		[(fl> t 1.0)]
	  (let ([p (eval-bezier-spline points t)])
		(raylib:play-sound (sebundle-bell sounds))
		(-> (spawn-bullet 'pellet-blue (v2x p) (v2y p) 5 values)
			(bullet-addflags (bltflags uncancelable)))
		(wait 4))))
  (define (heart task)
	(define n 20)
	(define angper (fl/ tau (fx2fl n)))
	(define (body i)
	  (let* ([t (fl* (fx2fl i) angper)]
			 [x (fl* 16.0 (flexpt (flsin t) 3.0))]
			 [y (fl- (fl* 13.0 (flcos t))
					 (fl* 5.0 (flcos (fl* 2.0 t)))
					 (fl* 2.0 (flcos (fl* 3.0 t)))
					 (flcos (fl* 4.0 t)))])
		(raylib:play-sound (sebundle-bell sounds))
		(-> (spawn-bullet 'pellet-red
						  (fl* 1.5 x)
						  (fl+ (fl* -1.5 y) 40.0)
						  5 values)
			(bullet-addflags (bltflags uncancelable)))
		(wait 2)))
	(spawn-subtask "second half"
	  (λ (task)
		(do [(i (quotient n 2) (add1 i))]
			[(= i n)]
		  (body i)))
	  task)
	(do [(i (sub1 (quotient n 2)) (sub1 i))]
		[(fxnegative? i)]
	  (body i))
	(-> (spawn-bullet 'small-star-white 0.0 42.0 5 values)
		(bullet-addflags (bltflags uncancelable))))
  (define right-x group-sp3-right-x)
  (define left-x group-sp3-left-x)
  (define top-y group-sp3-top-y)
  (define bot-y group-sp3-bot-y)
  (parameterize ([ovr-uncancelable #t])
	(spawn-bullet 'big-star-magenta left-x top-y 5 values)
	(spawn-bullet 'big-star-magenta right-x top-y 5 values)
	(spawn-bullet 'big-star-magenta left-x bot-y 5 values)
	(spawn-bullet 'big-star-magenta right-x bot-y 5 values))
  (spawn-subtask "top left"
	(λ (task)
	  (ring left-x top-y 15 6)
	  (lace-horiz (vector (vec2 left-x (fl+ top-y ringrad))
						  (vec2 (fl+ left-x 40.0) (fl+ top-y ringrad -10.0))
						  (vec2 (fl+ left-x 40.0) (fl+ top-y (fl- ringrad) 10.0))
						  (vec2 (fl+ left-x 80.0) (fl+ top-y (fl- ringrad) 10.0))
						  (vec2 (fl+ left-x 120.0) (fl+ top-y (fl- ringrad) 10.0))
						  (vec2 0.0 top-y)
						  (vec2 0.0 top-y)))
	  (heart task))
	task)
  (spawn-subtask "top right"
	(λ (task)
	  (ring right-x top-y 21 30)
	  (lace-horiz (vector (vec2 right-x (fl+ top-y ringrad))
						  (vec2 (fl- right-x 40.0) (fl+ top-y ringrad -10.0))
						  (vec2 (fl- right-x 40.0) (fl+ top-y (fl- ringrad) 10.0))
						  (vec2 (fl- right-x 80.0) (fl+ top-y (fl- ringrad) 10.0))
						  (vec2 (fl- right-x 120.0) (fl+ top-y (fl- ringrad) 10.0))
						  (vec2 0.0 top-y)
						  (vec2 0.0 top-y))))
	task)
  (spawn-subtask "bottom left"
	(λ (task)
	  (ring left-x bot-y 9 18)
	  (lace-horiz (vector (vec2 left-x (fl- bot-y ringrad))
						  (vec2 (fl+ left-x 40.0) (fl+ bot-y (fl- ringrad) 10.0))
						  (vec2 (fl+ left-x 40.0) (fl+ bot-y ringrad -10.0))
						  (vec2 (fl+ left-x 80.0) (fl+ bot-y ringrad -10.0))
						  (vec2 (fl+ left-x 120.0) (fl+ bot-y ringrad -10.0))
						  (vec2 0.0 bot-y)
						  (vec2 0.0 bot-y))))
	task)
  (spawn-subtask "bottom right"
	(λ (task)
	  (ring right-x bot-y 27 18)
	  (lace-horiz (vector (vec2 right-x (fl- bot-y ringrad))
						  (vec2 (fl- right-x 40.0) (fl+ bot-y (fl- ringrad) 10.0))
						  (vec2 (fl- right-x 40.0) (fl+ bot-y ringrad -10.0))
						  (vec2 (fl- right-x 80.0) (fl+ bot-y ringrad -10.0))
						  (vec2 (fl- right-x 120.0) (fl+ bot-y ringrad -10.0))
						  (vec2 0.0 bot-y)
						  (vec2 0.0 bot-y))))
	task)
  (spawn-subtask "left bottom"
	(λ (task)
	  (ring left-x bot-y 9 0)
	  (lace-horiz (vector (vec2 (fl+ left-x ringrad) bot-y)
						  (vec2 (fl+ left-x 10.0) (fl- bot-y 60.0))
						  (vec2 (fl- left-x 20.0) (fl- bot-y 60.0))
						  (vec2 (fl- left-x 20.0) (fl- bot-y 100.0))
						  (vec2 (fl- left-x 20.0) (fl- bot-y 140.0))
						  (vec2 left-x 248.0)
						  (vec2 left-x 248.0))))
	task)
  (spawn-subtask "left top"
	(λ (task)
	  (ring left-x top-y 15 24)
	  (lace-horiz (vector (vec2 (fl+ left-x ringrad) top-y)
						  (vec2 (fl+ left-x 10.0) (fl+ top-y 60.0))
						  (vec2 (fl- left-x 20.0) (fl+ top-y 60.0))
						  (vec2 (fl- left-x 20.0) (fl+ top-y 100.0))
						  (vec2 (fl- left-x 20.0) (fl+ top-y 140.0))
						  (vec2 left-x 248.0)
						  (vec2 left-x 248.0))))
	task)
  (spawn-subtask "right bottom"
	(λ (task)
	  (ring right-x bot-y 3 12)
	  (lace-horiz (vector (vec2 (fl- right-x ringrad) bot-y)
						  (vec2 (fl- right-x 10.0) (fl- bot-y 60.0))
						  (vec2 (fl+ right-x 20.0) (fl- bot-y 60.0))
						  (vec2 (fl+ right-x 20.0) (fl- bot-y 100.0))
						  (vec2 (fl+ right-x 20.0) (fl- bot-y 140.0))
						  (vec2 right-x 248.0)
						  (vec2 right-x 248.0))))
	task)
  (spawn-subtask "right top"
	(λ (task)
	  (ring right-x top-y 21 12)
	  (lace-horiz (vector (vec2 (fl- right-x ringrad) top-y)
						  (vec2 (fl- right-x 10.0) (fl+ top-y 60.0))
						  (vec2 (fl+ right-x 20.0) (fl+ top-y 60.0))
						  (vec2 (fl+ right-x 20.0) (fl+ top-y 100.0))
						  (vec2 (fl+ right-x 20.0) (fl+ top-y 140.0))
						  (vec2 right-x 248.0)
						  (vec2 right-x 248.0))))
	task))

(define group-sp3-flower-center (cons 0.0 248.0)) ;; mutated
(define (group-sp3 task aiko)
  (define bossinfo (blank-doremi-bossinfo))
  (define (keep-running)
	(fxpositive? (bossinfo-remaining-timer bossinfo)))
  (define _ (wait 60))
  (define doremi
	(spawn-enemy (enmtype boss-doremi) 100.0 -100.0 500
				 (λ (task enm)
				   (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+ 80 enm))
				 '()
				 (constantly #f)))
  (define hazuki
	(spawn-enemy (enmtype boss-hazuki) -100.0 -100.0 500
				 (λ (task enm)
				   (ease-to ease-out-cubic +left-boss-x+ +left-boss-y+ 80 enm))
				 '()
				 (constantly #f)))
  (define spin-speed (cons 0.0 0.0))
  (define flower-distmod (box 0.0))
  (set! current-chapter 30)
  (set-car! group-sp3-flower-center 0.0)
  (set-cdr! group-sp3-flower-center 248.0)
  (enm-extras-set! doremi bossinfo)
  (enm-extras-set! hazuki (blank-hazuki-bossinfo))
  (enm-redirect-damage-set! hazuki doremi)
  (enm-redirect-damage-set! aiko doremi)
  (for-each (λ (e) (enm-addflags e (enmflags nocollide)))
			(list doremi hazuki aiko))
  (ease-to ease-out-cubic +right-boss-x+ +right-boss-y+ 80 aiko)
  (for-each-indexed
   (λ (i type)
	 (-> (cb)
		 (cbcount 36)
		 (cboffset 250.0)
		 (cbabsolute-aim)
		 (cbang (fx2fl (* i 15)))
		 (cbshootez type 0.0 248.0 5 (sebundle-shoot0 sounds)
					(λ (facing _speed task blt)
					  (linear-step-accelerate (fl+ pi facing) 0.0 0.02 1.1 blt)
					  (linear-step-forever (fl+ pi facing) 1.1 task blt))))
	 (wait (vnth '#(30 15 120) i)))
   '(big-star-red big-star-orange big-star-blue))
  (raylib:play-sound (sebundle-longcharge sounds))
  (spawn-subtask "hazuki exit"
	(λ (task)
	  (ease-to ease-out-cubic -200.0 -150.0 60 hazuki))
	task)
  (spawn-subtask "aiko exit"
	(λ (task)
	  (ease-to ease-out-cubic 200.0 -150.0 60 aiko))
	task)
  (ease-to ease-out-cubic -200.0 -150.0 60 doremi)
  (cancel-all #f)
  (bossinfo-healthbars-set!
   bossinfo
   (vector-pop (bossinfo-healthbars (enm-extras aiko))))
  ;; widen to full width
  (healthbar-width-set! (vnth (bossinfo-healthbars bossinfo)
							  (sub1 (vlen (bossinfo-healthbars bossinfo))))
						-1)
  (declare-spell doremi 10)
  (wait 45)
  (group-sp3-setup-field task)
  (let ([angper (fl* 3.0 pi 0.2)]
		[flower-ctrl
		 (λ (backward task blt)
		   (define initial-ang (facing-point 0.0 248.0 (bx blt) (by blt)))
		   (define initial-dist (flsqrt (distsq 0.0 248.0 (bx blt) (by blt))))
		   (let loop ([ang initial-ang])
			 (let*-values ([(omega)
							(torad (if backward (car spin-speed) (cdr spin-speed)))]
						   [(new-ang) (fl+ ang omega)]
						   [(x y) (dist-away
								   (car group-sp3-flower-center)
								   (cdr group-sp3-flower-center)
								   new-ang
								   (fl+ initial-dist (unbox flower-distmod)))])
			   (bullet-x-set! blt x)
			   (bullet-y-set! blt y)
			   (yield)
			   (loop new-ang))))])
	(do [(petal 0 (add1 petal))]
		[(= petal 10)]
	  (spawn-subtask "spawn petal"
		(λ (task)
		  (when (fxeven? petal)
			(wait 50))
		  (let* ([start-ang (fl* (fx2fl petal) angper)]
				 [end-ang (fl+ start-ang angper)])
			(do [(t 0.0 (fl+ t (inexact 1/30)))]
				[(fl> t 1.0)]
			  (let-values ([(x y)
							(let* ([theta (lerp start-ang end-ang
											   (if (fxeven? petal) t (fl- 1.0 t)))]
								   [r (fl+ 50.0 (fl* 80.0 (flasin (flabs (flsin (* 5/3 theta))))))])
							  (dist-away 0.0 248.0 theta r))])
				(raylib:play-sound (sebundle-shootsoft sounds))
				(-> (spawn-bullet
					 (if (fxeven? petal) 'border-ball-red 'border-ball-orange)
					 x y 2 (curry flower-ctrl (fxeven? petal)) -2)
					(bullet-addflags (bltflags uncancelable)))
				(wait 2)))))
		task)))
  (wait 120)
  (raylib:play-sound (sebundle-shortcharge sounds))
  (wait 60)
  (raylib:play-sound (sebundle-release sounds))
  (set-car! spin-speed -0.5)
  (set-cdr! spin-speed 0.7)
  (vector-for-each
   (λ (x y init-ang)
	 (define winding (flcopysign 55.0 x))
	 (spawn-subtask "corner"
	   (λ (task)
		 (let loop ([ang init-ang])
		   (-> (fb)
			   (fbcount 4)
			   (fbabsolute-aim)
			   (fbang ang 10.0)
			   (fbspeed 1.25)
			   (fbshootez
				;; really arrowhead-green, the sprite data is wrong but I'm too lazy
				;; to deal with it right for this game lol
				'arrowhead-cyan x y 8 (sebundle-shootsoft sounds)))
		   (wait 45)
		   (loop (fl+ ang winding))))
	   task keep-running))
   (vector group-sp3-left-x group-sp3-right-x group-sp3-left-x group-sp3-right-x)
   (vector group-sp3-top-y group-sp3-top-y group-sp3-bot-y group-sp3-bot-y)
   '#(0.0 135.0 180.0 -135.0))
  (wait 90)

  (spawn-subtask "main flow"
	(λ (task)
	  (define heart-y 42.0)
	  (define (w1 backward)
		(do [(i 0 (add1 i))]
			[(= i 4)]
		  (-> (fb)
			  (fbcount 5)
			  (fbspeed 2.5)
			  (fbabsolute-aim)
			  (fbang (if backward
						 (fl- 110.0 (fl* (fx2fl i) 10.0))
						 (fl+ 60.0 (fl* (fx2fl i) 10.0)))
					 15.0)
			  (fbshootez
			   (vnth '#(knife-red knife-orange knife-blue knife-magenta) i)
			   0.0 heart-y 5 (sebundle-shoot0 sounds)
			   (λ (facing speed task blt)
				 (linear-step-decelerate-to facing speed -0.05 0.0 blt)
				 (wait 30)
				 (raylib:play-sound (sebundle-bell sounds))
				 (linear-step-forever (facing-player (bx blt) (by blt))
									  4.0 task blt))))
		  (wait 15)))
	  (define (w2 left)
		(define start-y (if left 185.0 200.0))
		(define x (if left group-sp3-left-x group-sp3-right-x))
		(define final-facing (if left 0.0 pi))
		(do [(i 0 (add1 i))]
			[(= i 6)]
		  (let ([dest-y (fl+ start-y (fx2fl (* 30 i)))])
			(raylib:play-sound (sebundle-shoot0 sounds))
			(-> (spawn-bullet
				 (vnth-mod
				  '#(glow-orb-red glow-orb-orange glow-orb-cyan
								  glow-orb-magenta glow-orb-yellow) i)
				 0.0 heart-y 5
				 (λ (task blt)
				   (ease-bullet-to ease-in-out-quad x dest-y 120 blt)
				   (when (zero? i)
					 (raylib:play-sound (sebundle-shortcharge sounds)))
				   (wait 60)
				   (when (zero? i)
					 (raylib:play-sound (sebundle-release sounds)))
				   (cancel-bullet blt)
				   (-> (fb)
					   (fbcount 1 4)
					   (fbabsolute-aim)
					   (fbang (todeg final-facing))
					   (fbspeed 3.0 4.0)
					   (fbshootez
						(vnth-mod
						 '#(knife-red knife-orange knife-blue
									  knife-magenta knife-yellow) i)
						(bx blt) (by blt) 0 (sebundle-bell sounds)))))
				(bullet-facing-set! (facing-point 0.0 heart-y x dest-y)))
			(wait 3))))
	  (wait 120)
	  (w1 #t) (wait 60) (w1 #f) (wait 90)
	  (w2 #t) (wait 60) (w2 #f) (wait 60)
	  (w2 #t) (wait 60) (w2 #f) (wait 180)
	  (spawn-subtask "hearts"
		(λ (task)
		  (wait 90)
		  (do [(i 0 (add1 i))]
			  [(= i 13)]
			(-> (cb)
				(cbcount 18 3)
				(cbspeed 3.25 4.0)
				(cbang 0.0 10.0)
				(cbrenderprio -1)
				(cbshootez 'heart-red 0.0 heart-y 5 (sebundle-bell sounds)))
			(when (= i 12)
			  (raylib:play-sound (sebundle-shortcharge sounds)))
			(wait 60))
		  (raylib:play-sound (sebundle-release sounds))		  
		  (do [(i 0 (add1 i))]
			  [(= i 18)]
			(-> (fb)
				(fbcount 3 2)
				(fbang 0.0 30.0)
				(fbspeed 2.0 3.6)
				(fbrenderprio -1)
				(fbshootez 'heart-orange 0.0 heart-y 5 (sebundle-bell sounds)))
			(when (= i 17)
			  (raylib:play-sound (sebundle-shortcharge sounds)))
			(wait 50))
		  (wait 10)
		  (raylib:play-sound (sebundle-release sounds))
		  (interval-loop 25
			(-> (cb)
				(cbcount 16)
				(cbang 11.25)
				(cbspeed 4.0 5.0)
				(cbrenderprio -1)
				(cbshootez 'heart-blue 0.0 heart-y 5 (sebundle-bell sounds)))))
		task)
	  (vector-for-each
	   (λ (x y)
		 (raylib:play-sound (sebundle-oldvwoopslow sounds))
		 (ease-to-impl car cdr set-car! set-cdr!
					   values x y 300 group-sp3-flower-center)
		 (unless (flzero? x)
		   (wait 130)))
	   '#(-60.0 60.0 -60.0 60.0 0.0)
	   '#(312.0 312.0 184.0 184.0 248.0))
	  (do [(i 0 (add1 i))]
		  [(= i 600)]
		(set-box! flower-distmod (fl* (fx2fl i) -0.08))
		(yield))
	  (loop-forever))
	task keep-running)
  (wait-while keep-running)
  (common-spell-postlude bossinfo doremi)
  (enm-clrflags doremi (enmflags invincible nocollide))
  (enm-clrflags hazuki (enmflags invincible nocollide))
  (enm-clrflags aiko (enmflags invincible nocollide))
  (group-sp4 task doremi hazuki aiko))

(define (group-sp4 task doremi hazuki aiko)
  (define center-y 120.0)
  (define bossinfo (enm-extras doremi))
  (define (keep-running)
	(and (fxpositive? (bossinfo-remaining-timer bossinfo))
		 (fxpositive? (enm-health doremi))))
  (define (p2)
	(or (fx< (bossinfo-remaining-timer bossinfo) (* 85 60))
		(fx< (enm-health doremi) 25000)))
  (define (p3)
	(or (fx< (bossinfo-remaining-timer bossinfo) (* 70 60))
		(fx< (enm-health doremi) 18000)))
  (define (p4)
	(or (fx< (bossinfo-remaining-timer bossinfo) (* 20 60))
		(fx< (enm-health doremi) 7500)))
  (set! current-chapter 31)
  (wait 90)
  (stage-ctx-dialogue-set!
   current-stage-ctx
   (with-input-from-file "assets/dialogue/finalspell.dat" read))
  (stage-ctx-dialogue-idx-set! current-stage-ctx 0)
  (wait-until (thunk (not (stage-ctx-dialogue current-stage-ctx))))
  (bossinfo-healthbars-set!
   bossinfo
   (vector-pop (bossinfo-healthbars bossinfo)))
  ;; widen to full width
  (healthbar-width-set! (vnth (bossinfo-healthbars bossinfo)
							  (sub1 (vlen (bossinfo-healthbars bossinfo))))
						-1)
  (enm-redirect-damage-set! hazuki doremi)
  (enm-redirect-damage-set! aiko doremi)

  (declare-spell doremi 11)
  (raylib:play-sound (sebundle-shortcharge sounds))
  (wait 90)
  (raylib:play-sound (sebundle-release sounds))
  (let* ([ring-rad 8.0]
		 [ring (map (λ (i type)
					  (define init-ang (fl+ -hpi (torad (fx2fl (* i 120)))))
					  (define-values (x y)
						(dist-away 0.0 center-y init-ang ring-rad))
					  (-> (spawn-bullet type x y 0 values)
						  (bullet-addflags (bltflags uncancelable))))
					(iota 3)
					'(music-red music-yellow music-cyan))])
	(spawn-subtask "boss spin"
	  (λ (task)
		(define radius 50.0)
		(let loop ([ang -hpi])
		  (enm-set-dist-away doremi 0.0 center-y ang radius)
		  (enm-set-dist-away aiko 0.0 center-y (fl+ ang (torad 120.0)) radius)
		  (enm-set-dist-away hazuki 0.0 center-y
							 (fl+ ang (torad 240.0)) radius)
		  (position-bullets-around 0.0 center-y ring-rad ang ring)
		  (yield)
		  (loop (fl+ ang (torad 1.0)))))
	  task keep-running))
  (spawn-subtask "revspin bullets"
	(λ (task)
	  (do [(ang 0.0 (fl- ang 5.1))
		   (i 0 (add1 i))]
		  [#f]
		(-> (cb)
			(cbcount (cond
					  [(p4) 12] [(p3) 10]
					  [(p2) 8] [else 6]))
			(cbspeed 4.0)
			(cbabsolute-aim)
			(cbang ang)
			(cboffset 20.0)
			(cbshootez
			 'music-blue 0.0 center-y 5 (sebundle-shootsoft sounds)
			 (λ (facing speed task blt)
			   (dotimes 15
				 (linear-step facing speed blt)
				 (yield))
			   (bullet-type-set! blt 'music-green)
			   (dotimes 15
				 (linear-step facing speed blt)
				 (yield))
			   (bullet-type-set! blt 'music-red)
			   (linear-step-forever facing speed task blt))))
		(wait (cond
			   [(or (p4) (p3)) 6]
			   [(p2) 8] [else 10]))))
	task keep-running)
  (spawn-subtask "p2+ rings"
	(λ (task)
	  (wait-until p2)
	  (raylib:play-sound (sebundle-shoot0 sounds))
	  (do [(i 0 (add1 i))]
		  [#f]
		(raylib:play-sound (sebundle-bell sounds))
		(-> (cb)
			(cbcount (cond
					  [(p4) 36] [(p3) 30]
					  [(p2) 24] [else 18]))
			(cbspeed 3.5)
			(cbshoot 0.0 center-y
			  (λ (layer in-layer speed facing)
				(define-values (x y)
				  (dist-away 0.0 center-y facing 20.0))
				(define type
				  (vnth-mod
				   '#(small-ball-red small-ball-yellow small-ball-blue) i))
				(define glow-type
				  (vnth-mod
				   '#(glow-orb-red glow-orb-yellow glow-orb-cyan) i))
				(define use-glow-type
				  (cond
				   [(or (p4) (p3)) (fx= 3 (fxmod in-layer 6))]
				   [else (fxzero? (fxmod in-layer 6))]))
				(spawn-bullet
				 (if use-glow-type glow-type type)
				 x y 5 (curry linear-step-forever facing speed)))))
		(wait (cond
			   [(or (p4) (p3)) 45]
			   [else 60]))))
	task keep-running)
  (spawn-subtask "p3"
	(λ (task)
	  (wait-until p3)
	  (raylib:play-sound (sebundle-shoot0 sounds))
	  (do [(i 0 (add1 i))]
		  [#f]
		(dotimes 8
		  (vector-for-each
		   (λ (e)
			 (define facing (facing-point 0.0 center-y (ex e) (ey e)))
			 (spawn-bullet
			  (vnth-mod '#(kunai-red kunai-orange kunai-blue) i)
			  (ex e) (ey e) 5 (curry linear-step-forever facing 4.2)))
		   (vector doremi hazuki aiko))
		  (wait 3))
		(wait 8)))
	task keep-running)
  (spawn-subtask "p4"
	(λ (task)
	  (wait-until p4)
	  (raylib:play-sound (sebundle-longcharge sounds))
	  (interval-loop 15
		(-> (cb)
			(cbcount 18)
			(cbspeed 5.0)
			(cbabsolute-aim)
			(cbshootez 'ellipse-blue 0.0 center-y 5 #f))))
	task keep-running)
  
  (wait-while keep-running)
  (common-spell-postlude bossinfo doremi)
  (spawn-subtask "hazuki postlude"
	(λ (_) (common-boss-postlude bossinfo hazuki #f))
	task)
  (spawn-subtask "aiko postlude"
	(λ (_) (common-boss-postlude bossinfo aiko #f))
	task)
  (common-boss-postlude bossinfo doremi #f)
  (wait 120)
  (stage-ctx-dialogue-set!
   current-stage-ctx
   (with-input-from-file "assets/dialogue/postbattle.dat" read))
  (stage-ctx-dialogue-idx-set! current-stage-ctx 0)
  (wait-until (thunk (not (stage-ctx-dialogue current-stage-ctx))))
  (let ([clear-bonus (+ (* 10000000 (floor life-stock))
						(* 1000000 (floor bomb-stock))
						10000000)])
	(set! current-score (+ current-score clear-bonus))
	(spawn-particle
	 (particletype clear-bonus)
	 ;; Position dynamically calculated at render to avoid
	 ;; needing to access the fonts here
	 0.0 75.0 240
	 (format "Clear Bonus: ~:d" clear-bonus)))
  (when (is-liveplay)
	(let ([pair (assq 'games-cleared play-data)])
	  (set-cdr! pair (add1 (cdr pair))))
	(save-play-data play-data))
  (wait 300)
  (replace-gui (mk-pause-gui
				(if (is-liveplay)
					(pausetype gameclear)
					(pausetype replaydone)))))
