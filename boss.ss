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
   (make-healthbar 4 1 #x00ffffff #x008b8bff)
   ;; hazuki sp2
   (make-healthbar 4 1 #xffa500ff #xf4a460ff)
   ;; doremi sp2
   (make-healthbar 4 1 #xff69fcff #xba55d3ff)
   ;; group sp2
   (make-healthbar 4 1 #xffd700ff #xdaa520ff)
   ;; aiko sp1
   (make-healthbar 4 1 #x00ffffff #x008b8bff)
   ;; hazuki sp1
   (make-healthbar 4 1 #xffa500ff #xf4a460ff)
   ;; doremi sp1
   (make-healthbar 4 1 #xff69fcff #xba55d3ff)
   ;; group sp1
   (make-healthbar 25 0 #xffd700ff #xdaa520ff)))

(define (group-non1 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (define keep-running
	(thunk
	 (and (positive? (bossinfo-remaining-timer bossinfo))
		  (positive? (enm-health doremi)))))
  (set! current-chapter 14)
  (play-music (musbundle-naisho-yo-ojamajo music))
  (bossinfo-healthbars-set!
   bossinfo
   (vector-add (base-healthbars) (make-non-healthbar)))
  (bossinfo-redirect-damage-set!
   (enm-extras hazuki) doremi)
  (bossinfo-redirect-damage-set!
   (enm-extras aiko) doremi)
  (declare-nonspell doremi 1800 5000)

  (spawn-subtask "doremi wander"
	(λ (task)
	  (wait (roll game-rng 60))
	  (interval-loop 60
		(boss-standard-wander-once doremi 10 40 50)))
	keep-running task)
  (spawn-subtask "hazuki wander"
	(λ (task)
	  (wait (roll game-rng 60))
	  (interval-loop 60
		(boss-standard-wander-once hazuki 10 40 50)))
	keep-running task)
  (spawn-subtask "aiko wander"
	(λ (task)
	  (wait (roll game-rng 60))
	  (interval-loop 60
		(boss-standard-wander-once aiko 10 40 50)))
	keep-running task)

  (wait 100)
  (-> (cb)
	  (cbcount 36)
	  (cbspeed 0.2)
	  (cbshootenm doremi 'fireball-magenta 5 (sebundle-shoot0 sounds)))
  (wait 40)
  (-> (cb)
	  (cbcount 36)
	  (cbspeed 1.0)
	  (cbshootenm doremi 'fireball-green 5 (sebundle-shoot0 sounds)))
  
  (wait-while keep-running)
  (common-nonspell-postlude bossinfo)
  (group-sp1 task doremi hazuki aiko))


(define (group-sp1 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (set! current-chapter 15)
  (bossinfo-redirect-damage-set!
   (enm-extras hazuki) doremi)
  (bossinfo-redirect-damage-set!
   (enm-extras aiko) doremi)
  (declare-spell doremi 2)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi)))))
  (common-spell-postlude bossinfo doremi)
  (doremi-non1 task doremi hazuki aiko))

(define (doremi-non1 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (set! current-chapter 16)
  (wait 90)
  (adjust-bars-non (bossinfo-healthbars bossinfo))
  (spawn-subtask "hazuki leave"
	(λ (_)
	  (ease-to ease-out-cubic -100.0 -100.0 60 hazuki)
	  (delete-enemy hazuki))
	(constantly #t)
	task)
  (spawn-subtask "aiko leave"
	(λ (_)
	  (ease-to ease-out-cubic 100.0 -100.0 60 aiko)
	  (delete-enemy aiko))
	(constantly #t)
	task)
  (declare-nonspell doremi 1800 1000)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi)))))
  (common-nonspell-postlude bossinfo)
  (doremi-sp1 task doremi))

(define (doremi-sp1 task doremi)
  (define bossinfo (enm-extras doremi))
  (set! current-chapter 17)
  (declare-spell doremi 3)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi)))))
  (common-spell-postlude bossinfo doremi)
  (hazuki-non1 task doremi))

(define (hazuki-non1 task doremi)
  (define bars (bossinfo-healthbars (enm-extras doremi)))
  (define _ (wait 90))
  (define hazuki
	(spawn-enemy (enmtype boss-hazuki) 100.0 -100.0 500
				 (λ (task enm)
				   (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+
							60 enm))
				 '()
				 (thunk #f)))
  (define bossinfo (blank-hazuki-bossinfo))
  (set! current-chapter 18)
  (spawn-subtask "doremi leave"
	(λ (_)
	  (ease-to ease-out-cubic -100.0 -100.0 60 doremi)
	  (delete-enemy doremi))
	(constantly #t)
	task)
  (adjust-bars-non bars)
  (bossinfo-healthbars-set! bossinfo bars)
  (enm-extras-set! hazuki bossinfo)
  (declare-nonspell hazuki 1800 1000)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health hazuki)))))
  (common-nonspell-postlude bossinfo)
  (hazuki-sp1 task hazuki))

(define (hazuki-sp1 task hazuki)
  (define bossinfo (enm-extras hazuki))
  (set! current-chapter 19)
  (declare-spell hazuki 4)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health hazuki)))))
  (common-spell-postlude bossinfo hazuki)
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
				 (thunk #f)))
  (define bossinfo (blank-aiko-bossinfo))
  (set! current-chapter 20)
  (spawn-subtask "hazuki leave"
	(λ (_)
	  (ease-to ease-out-cubic -100.0 -100.0 60 hazuki)
	  (delete-enemy hazuki))
	(constantly #t)
	task)
  (adjust-bars-non bars)
  (bossinfo-healthbars-set! bossinfo bars)
  (enm-extras-set! aiko bossinfo)
  (declare-nonspell aiko 1800 1000)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health aiko)))))
  (common-nonspell-postlude bossinfo)
  (aiko-sp1 task aiko))

(define (aiko-sp1-round task aiko)
  (define spread-signal (box #f))
  (do [(i 0 (add1 i))]
	  [(= i 3)]
	(let ([winding (if (roll-bool game-rng) 1.0 -1.0)]
		  [colors (vrand
				   '#(#(small-star-blue small-star-cyan)
					  #(small-star-orange small-star-white)
					  #(small-star-red small-star-magenta))
				   game-rng)]
		  [x (case i
			   [(0) (fx2fl (roll-range game-rng -115 -80))]
			   [(1) (fx2fl (roll-range game-rng 80 115))]
			   [(2) (centered-roll game-rng 115.0)])]
		  [y (vnth '#(150.0 230.0 300.0) i)])
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
		  (cbshootenm aiko 'medium-ball-blue 2 (sebundle-shoot0 sounds)))
	  (-> (cb)
		  (cbcount 16)
		  (cbspeed 4.0)
		  (cbang 11.25)
		  (cbshootenm aiko 'medium-ball-cyan 2 #f))
	  (->
	   (spawn-bullet
		'big-star-red x y 2
		(λ (blt)
		  (-> (cb)
			  (cbcount 9 17)
			  (cbspeed 1.8 4.0)
			  (cbabsolute-aim)
			  (cbang (centered-roll game-rng 180.0) (fl* winding 4.0))
			  (cbshoot x y
				(λ (layer in-layer speed facing)
				  (->
				   (spawn-bullet
					(vnth-mod colors in-layer)
					x y 10
					(λ (blt)
					  (linear-step-decelerate facing speed -0.10 blt)
					  (wait-until (thunk (unbox spread-signal)))
					  (bullet-clrflags blt (bltflags uncancelable))
					  (wait (* 10 layer))
					  (linear-step-accelerate (fl+ facing pi) 0.0 0.02 4.5 blt)
					  (linear-step-forever (fl+ facing pi) 4.5 blt)))
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
				(let ([x (fl+ (enm-x aiko) (centered-roll game-rng 50.0))]
					  [y (fl+ (enm-y aiko) (centered-roll game-rng 10.0))])
				  (ease-to ease-in-out-quad x y 30 aiko)
				  (wait 30))))
			(constantly #t)
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
			(constantly #t)
			move-task)])
	(wait-until (thunk (task-dead move-task)))))

(define (aiko-sp1 task aiko)
  (define bossinfo (enm-extras aiko))
  (define keep-running
	(thunk
	 (and (positive? (bossinfo-remaining-timer bossinfo))
		  (positive? (enm-health aiko)))))
  (set! current-chapter 21)
  (declare-spell aiko 5)
  (wait 100)
  (loop-while
   (keep-running)
   (let ([t (spawn-subtask "wave"
			  (λ (task)
				(aiko-sp1-round task aiko)
				(wait 120))
			  keep-running
			  task)])
	 (wait-until (thunk (task-dead t)))))
  (common-spell-postlude bossinfo aiko)
  (group-non2 task aiko))

(define (group-non2 task aiko)
  (define bossinfo (blank-doremi-bossinfo))
  (define _ (wait 90))
  (define doremi
	(spawn-enemy (enmtype boss-doremi) 100.0 -100.0 500
				 (λ (task enm)
				   (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+
							60 enm))
				 '()
				 (thunk #f)))
  (define hazuki
	(spawn-enemy (enmtype boss-hazuki) -100.0 -100.0 500
				 (λ (task enm)
				   (ease-to ease-out-cubic +left-boss-x+ +left-boss-y+
							60 enm))
				 '()
				 (thunk #f)))
  (set! current-chapter 22)
  (enm-extras-set! doremi bossinfo)
  (enm-extras-set! hazuki (blank-hazuki-bossinfo))
  (bossinfo-healthbars-set!
   bossinfo
   (adjust-bars-non (bossinfo-healthbars (enm-extras aiko))))
  (bossinfo-healthbars-set! (enm-extras aiko) '#())
  (bossinfo-redirect-damage-set!
   (enm-extras hazuki) doremi)
  (bossinfo-redirect-damage-set!
   (enm-extras aiko) doremi)
  (ease-to ease-out-cubic +right-boss-x+ +right-boss-y+ 60 aiko)
  (declare-nonspell doremi 1800 1000)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi)))))
  (common-nonspell-postlude bossinfo)
  (group-sp2 task doremi hazuki aiko))

(define (group-sp2 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (set! current-chapter 23)
  (bossinfo-redirect-damage-set!
   (enm-extras hazuki) doremi)
  (bossinfo-redirect-damage-set!
   (enm-extras aiko) doremi)
  (declare-spell doremi 6)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi)))))
  (common-spell-postlude bossinfo doremi)
  (doremi-non2 task doremi hazuki aiko))

(define (doremi-non2 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (set! current-chapter 24)
  (wait 90)
  (adjust-bars-non (bossinfo-healthbars bossinfo))
  (spawn-subtask "hazuki leave"
	(λ (_)
	  (ease-to ease-out-cubic -100.0 -100.0 60 hazuki)
	  (delete-enemy hazuki))
	(constantly #t)
	task)
  (spawn-subtask "aiko leave"
	(λ (_)
	  (ease-to ease-out-cubic 100.0 -100.0 60 aiko)
	  (delete-enemy aiko))
	(constantly #t)
	task)
  (declare-nonspell doremi 1800 1000)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi)))))
  (common-nonspell-postlude bossinfo)
  (doremi-sp2 task doremi))

(define (doremi-sp2 task doremi)
  (define bossinfo (enm-extras doremi))
  (set! current-chapter 25)
  (declare-spell doremi 7)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi)))))
  (common-spell-postlude bossinfo doremi)
  (hazuki-non2 task doremi))

(define (hazuki-non2 task doremi)
  (define bars (bossinfo-healthbars (enm-extras doremi)))
  (define _ (wait 90))
  (define hazuki
	(spawn-enemy (enmtype boss-hazuki) 100.0 -100.0 500
				 (λ (task enm)
				   (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+
							60 enm))
				 '()
				 (thunk #f)))
  (define bossinfo (blank-hazuki-bossinfo))
  (set! current-chapter 26)
  (spawn-subtask "doremi leave"
	(λ (_)
	  (ease-to ease-out-cubic -100.0 -100.0 60 doremi)
	  (delete-enemy doremi))
	(constantly #t)
	task)
  (adjust-bars-non bars)
  (bossinfo-healthbars-set! bossinfo bars)
  (enm-extras-set! hazuki bossinfo)
  (declare-nonspell hazuki 1800 1000)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health hazuki)))))
  (common-nonspell-postlude bossinfo)
  (hazuki-sp2 task hazuki))

(define (hazuki-sp2 task hazuki)
  (define bossinfo (enm-extras hazuki))
  (define (keep-running)
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health hazuki))))
  (set! current-chapter 27)
  (declare-spell hazuki 8)
  ;; (-> (spawn-enemy (enmtype red-wisp) 0.0 200.0 500
  ;; 				   (λ (_task _enm) (void)))
  ;; 	  (enm-addflags (enmflags aura-red)))
  ;; (wait 30)
  ;; (-> (spawn-enemy (enmtype blue-wisp) 50.0 200.0 500
  ;; 				   (λ (_task _enm) (void)))
  ;; 	  (enm-addflags (enmflags aura-blue)))
  ;; (wait 30)
  ;; (-> (spawn-enemy (enmtype green-wisp) 100.0 200.0 500
  ;; 				   (λ (_task _enm) (void)))
  ;; 	  (enm-addflags (enmflags aura-green)))
  ;; (wait 30)
  ;; (-> (spawn-enemy (enmtype yellow-wisp) 150.0 200.0 500
  ;; 				   (λ (_task _enm) (void)))
  ;; 	  (enm-addflags (enmflags aura-magenta)))
  (wait 45)
  (ease-to ease-out-quad 0.0 215.0 45 hazuki)
  (raylib:play-sound (sebundle-laugh sounds))
  ;; friendly fire. hardcoded to the bullet types fired by the wisps.
  (spawn-subtask "damager"
	(λ (task)
	  (let-values ([(x y w h) (enm-collision-box hazuki)])
		(loop-forever
		 (vector-for-each-truthy
		  (λ (blt)
			(when (and (memq (bullet-family (bullet-type blt)) '(pellet small-ball))
					   (check-collision-circle-rec
						(bullet-x blt) (bullet-y blt)
						(bullet-hit-radius (bullet-type blt))
						x y w h))
			  (enm-health-set! hazuki (- (enm-health hazuki) 100))
			  (cancel-bullet blt)))
		  live-bullets))))
	keep-running task)
  (-> (spawn-enemy
	   (enmtype red-wisp) -100.0 200.0 500
	   (λ (task enm)
		 (interval-loop 5
		   (-> (fb)
			   (fbabsolute-aim)
			   (fbcount 1 5)
			   (fbspeed 1.0 4.0)
			   (fbang (flatan (fl- (enm-y hazuki) (enm-y enm))
							  (fl- (enm-x hazuki) (enm-x enm))))
			   (fbshootenm enm 'pellet-white 2 #f)))
		 (void)))
	  (enm-addflags (enmflags aura-red)))
  (wait-while keep-running)
  (common-spell-postlude bossinfo hazuki)
  (aiko-non2 task hazuki))

(define (aiko-non2-laser-ctrl is-right aiko blt)
  (define start-ang -hpi)
  (define dang (torad 160.0))
  (do [(i 0 (fx1+ i))]
	  [(fx= i 300)]
	(let-values ([(x) (enm-x aiko)]
				 [(y) (enm-y aiko)]
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

;; todo: put this attack somewhere else
#;(spawn-subtask "atk"
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
				  (cbspeed 4.0 4.75)
				  (cbcount 36 4)
				  (cbang 0.0 (if (roll-bool game-rng ) 3.0 -3.0))
				  (cbshootenm aiko 'music-cyan 5 (sebundle-bell sounds)))))
		(boss-standard-wander-once aiko 40 50 30)))
	keep-running? task)

(define (aiko-non2 task hazuki)
  (define bars (bossinfo-healthbars (enm-extras hazuki)))
  (define _ (wait 90))
  (define aiko
	(spawn-enemy (enmtype boss-aiko) 100.0 -100.0 500
				 (λ (task enm)
				   (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+
							60 enm))
				 '()
				 (thunk #f)))
  (define bossinfo (blank-aiko-bossinfo))
  (define (keep-running?)
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health aiko))))
  (set! current-chapter 28)
  (spawn-subtask "hazuki leave"
	(λ (_)
	  (ease-to ease-out-cubic -100.0 -100.0 60 hazuki)
	  (delete-enemy hazuki))
	(constantly #t)
	task)
  (adjust-bars-non bars)
  (bossinfo-healthbars-set! bossinfo bars)
  (enm-extras-set! aiko bossinfo)
  (declare-nonspell aiko 1800 6000)
  (wait 60)
  (raylib:play-sound (sebundle-laser sounds))
  (parameterize ([ovr-uncancelable #t])
	(for-each
	 (λ (is-right)
	   (spawn-laser 'fixed-laser-blue
					(enm-x aiko) (enm-y aiko)
					-hpi
					(fx2fl +playfield-height+)
					5.0
					40 30 (curry aiko-non2-laser-ctrl is-right aiko)))
	 '(#f #t)))
  (wait 30)
  (let* ([timeup (thunk (fx<= (bossinfo-remaining-timer bossinfo) 900))]
		 [rage (thunk (or (timeup) (fl<= player-y (fl+ (enm-y aiko) 100.0))))])
	(spawn-subtask "rage rings"
	  (λ (task)
		(interval-loop 23
		  (when (rage)
			(-> (cb)
				(cbcount 48)
				(cbspeed 4.0)
				(cbang (if (roll-bool game-rng) 0.0 2.0))
				(cbshootenm aiko 'kunai-red 2 (sebundle-shoot0 sounds))))))
	  keep-running? task)
	(spawn-subtask "rings"
	  (λ (task)
		(interval-loop-while 60 (fx<= (bossinfo-elapsed-frames bossinfo) 350)
		  (-> (cb)
			  (cbcount 12)
			  (cbspeed 3.0)
			  (cbshootenm aiko 'heart-blue 2 (sebundle-shoot0 sounds))))
		(raylib:play-sound (sebundle-shortcharge sounds))
		(spawn-subtask "decor"
		  (λ (task)
			(interval-loop 23
			  (-> (fb)
				  (fbcount 15)
				  (fbspeed 4.0)
				  (fbabsolute-aim)
				  (fbang -90.0 12.0)
				  (fbshootenm aiko 'big-star-orange 2 (sebundle-bell sounds)))))
		  keep-running? task)
		(interval-loop 46
		  (-> (fb)
			  (fbcount 7)
			  (fbspeed 3.0)
			  (fbang 0.0 8.0)
			  (fbshootenm aiko 'heart-blue 2 (sebundle-shoot0 sounds)))))
	  keep-running? task))
  (ease-to values +middle-boss-x+ 225.0 300 aiko)
  (wait-while keep-running?)
  (common-nonspell-postlude bossinfo)
  (aiko-sp2 task aiko))

(define aiko-sp2-x-margin 24)
(define aiko-sp2-x-left)
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

(define (aiko-sp2-ball-ctrl blt)
  (let loop ([facing (centered-roll game-rng pi)])
	(let ([ox (bullet-x blt)]
		  [oy (bullet-y blt)])
	  (linear-step facing 3.0 blt)
	  (yield)
	  (let ([nx (bullet-x blt)]
			[ny (bullet-y blt)]
			[xv (flcos facing)]
			[yv (flsin facing)])
		(cond
		 [(> ny (+ +playfield-max-y+ 10))
		  (damage-player)
		  (linear-step-forever facing 3.0 blt)]
		 [(or
		   ;; top
		   (and (> oy aiko-sp2-y-top)
				(<= ny aiko-sp2-y-top))
		   ;; bottom
		   (and (or (<= aiko-sp2-x-left nx aiko-sp2-goal-left)
					(<= aiko-sp2-goal-right nx aiko-sp2-x-right))
				(< oy aiko-sp2-y-bot)
				(>= ny aiko-sp2-y-bot)))
		  (loop (flatan (fl- yv) xv))]
		 [(or
		   ;; left
		   (and (> ox aiko-sp2-x-left)
				(<= nx aiko-sp2-x-left))
		   ;; right
		   (and (< ox aiko-sp2-x-right)
				(>= nx aiko-sp2-x-right))
		   ;; inner edges of the goal
		   (and (or (and (> ox aiko-sp2-goal-left)
						 (<= nx aiko-sp2-goal-left))
					(and (< ox aiko-sp2-goal-right)
						 (>= nx aiko-sp2-goal-right)))
				(>= ny aiko-sp2-y-bot)))
		  (loop (flatan yv (fl- xv)))]
		 [else (loop facing)])))))

(define (aiko-sp2 task aiko)
  (define bossinfo (enm-extras aiko))
  (define (keep-running?)
	(and (fxpositive? (bossinfo-remaining-timer bossinfo))
		 (fxpositive? (enm-health aiko))))
  (set! current-chapter 29)
  (declare-spell aiko 9)
  (wait 90)
  (raylib:play-sound (sebundle-longcharge sounds))
  (parameterize ([ovr-uncancelable #t])
	(let ([ctrl (λ (_blt) (loop-forever))])
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
	  (spawn-laser 'fixed-laser-blue -67.0 aiko-sp2-y-bot
				   pi (flabs (fl- aiko-sp2-x-left -67.0)) aiko-sp2-laser-radius
				   40 60 ctrl)
	  (spawn-laser 'fixed-laser-blue -67.0 aiko-sp2-y-bot
				   hpi 50.0 aiko-sp2-laser-radius
				   40 60 ctrl)
	  (spawn-laser 'fixed-laser-blue 67.0 aiko-sp2-y-bot
				   0.0 (fl- aiko-sp2-x-right 67.0) aiko-sp2-laser-radius
				   40 60 ctrl)
	  (spawn-laser 'fixed-laser-blue 67.0 aiko-sp2-y-bot
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
				   40 60 ctrl))
	;; (spawn-bullet 'yinyang-blue 0.0 100.0 5
	;; 			  aiko-sp2-ball-ctrl)
	)
  (wait 60)
  (raylib:play-sound (sebundle-laser sounds))
  (wait-while keep-running?)
  (common-spell-postlude bossinfo aiko)
  (group-sp3 task aiko))

(define (group-sp3 task aiko)
  (define bossinfo (blank-doremi-bossinfo))
  (define _ (wait 90))
  (define doremi
	(spawn-enemy (enmtype boss-doremi) 100.0 -100.0 500
				 (λ (task enm)
				   (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+
							60 enm))
				 '()
				 (thunk #f)))
  (define hazuki
	(spawn-enemy (enmtype boss-hazuki) -100.0 -100.0 500
				 (λ (task enm)
				   (ease-to ease-out-cubic +left-boss-x+ +left-boss-y+
							60 enm))
				 '()
				 (thunk #f)))
  (set! current-chapter 30)
  (enm-extras-set! doremi bossinfo)
  (enm-extras-set! hazuki (blank-hazuki-bossinfo))
  (bossinfo-redirect-damage-set!
   (enm-extras hazuki) doremi)
  (bossinfo-redirect-damage-set!
   (enm-extras aiko) doremi)
  (ease-to ease-out-cubic +right-boss-x+ +right-boss-y+ 60 aiko)
  (bossinfo-healthbars-set!
   bossinfo
   (vector-pop (bossinfo-healthbars (enm-extras aiko))))
  ;; widen to full width
  (healthbar-width-set! (vnth (bossinfo-healthbars bossinfo)
							  (sub1 (vlen (bossinfo-healthbars bossinfo))))
						-1)
  (declare-spell doremi 10)
  (enm-addflags doremi (enmflags invincible))
  (enm-addflags hazuki (enmflags invincible))
  (enm-addflags aiko (enmflags invincible))
  (wait-while
   (thunk (positive? (bossinfo-remaining-timer bossinfo))))
  (common-spell-postlude bossinfo doremi)
  (enm-clrflags doremi (enmflags invincible))
  (enm-clrflags hazuki (enmflags invincible))
  (enm-clrflags aiko (enmflags invincible))
  (group-sp4 task doremi hazuki aiko))

(define (group-sp4 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (set! current-chapter 31)
  (wait 90)
  (bossinfo-healthbars-set!
   bossinfo
   (vector-pop (bossinfo-healthbars bossinfo)))
  ;; widen to full width
  (healthbar-width-set! (vnth (bossinfo-healthbars bossinfo)
							  (sub1 (vlen (bossinfo-healthbars bossinfo))))
						-1)
  (bossinfo-redirect-damage-set!
   (enm-extras hazuki) doremi)
  (bossinfo-redirect-damage-set!
   (enm-extras aiko) doremi)
  (declare-spell doremi 11)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi)))))
  (common-spell-postlude bossinfo doremi)
  (spawn-subtask "hazuki postlude"
	(λ (_)
	  (common-boss-postlude bossinfo hazuki #f))
	(constantly #t)
	task)
  (spawn-subtask "aiko postlude"
	(λ (_)
	  (common-boss-postlude bossinfo aiko #f))
	(constantly #t)
	task)
  (common-boss-postlude bossinfo doremi #f)
  (wait 120)
  (stage-ctx-dialogue-set!
   current-stage-ctx
   (with-input-from-file "assets/dialogue/postbattle.dat" read))
  (stage-ctx-dialogue-idx-set! current-stage-ctx 0)
  (wait-until (thunk (not (stage-ctx-dialogue current-stage-ctx))))
  ;; todo formula
  (let ([clear-bonus 10000000])
	(set! current-score (+ current-score clear-bonus))
	(spawn-particle
	 (particletype clear-bonus)
	 ;; Position dynamically calculated at render to avoid
	 ;; needing to access the fonts here
	 0.0 0.0 240
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
