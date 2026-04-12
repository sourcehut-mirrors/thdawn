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
  (define (keep-running)
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi))))
  (set! current-chapter 14)
  (play-music (musbundle-naisho-yo-ojamajo music))
  (bossinfo-healthbars-set!
   bossinfo
   (vector-add (base-healthbars) (make-non-healthbar)))
  (enm-redirect-damage-set! hazuki doremi)
  (enm-redirect-damage-set! aiko doremi)
  (declare-nonspell doremi 1800 5000)

  (spawn-subtask "doremi wander"
	(λ (task)
	  (wait (roll game-rng 60))
	  (interval-loop 60
		(boss-standard-wander-once doremi 10 40 50)))
	task keep-running)
  (spawn-subtask "hazuki wander"
	(λ (task)
	  (wait (roll game-rng 60))
	  (interval-loop 60
		(boss-standard-wander-once hazuki 10 40 50)))
	task keep-running)
  (spawn-subtask "aiko wander"
	(λ (task)
	  (wait (roll game-rng 60))
	  (interval-loop 60
		(boss-standard-wander-once aiko 10 40 50)))
	task keep-running)

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
  (enm-redirect-damage-set! hazuki doremi)
  (enm-redirect-damage-set! aiko doremi)
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
	task)
  (spawn-subtask "aiko leave"
	(λ (_)
	  (ease-to ease-out-cubic 100.0 -100.0 60 aiko)
	  (delete-enemy aiko))
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
				 (constantly #f)))
  (define bossinfo (blank-hazuki-bossinfo))
  (set! current-chapter 18)
  (spawn-subtask "doremi leave"
	(λ (_)
	  (ease-to ease-out-cubic -100.0 -100.0 60 doremi)
	  (delete-enemy doremi))
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
				  (cbspeed 4.0 4.75)
				  (cbcount 36 4)
				  (cbang 0.0 (if (roll-bool game-rng) 3.0 -3.0))
				  (cbshoot (enm-x aiko) (enm-y aiko)
					(λ (layer in-layer speed facing)
					  (raylib:play-sound (sebundle-bell sounds))
					  (spawn-bullet
					   (vnth '#(music-yellow music-cyan music-blue music-magenta)
							 layer)
					   (enm-x aiko) (enm-y aiko) 5
					   (curry linear-step-forever facing speed))))
				  #;(cbshootenm aiko 'music-cyan 5 (sebundle-bell sounds)))))
		(boss-standard-wander-once aiko 40 50 30)))
	task keep-running)
  (wait-while keep-running)
  (common-nonspell-postlude bossinfo)
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
		(λ (task blt)
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
					(λ (task blt)
					  (linear-step-decelerate facing speed -0.10 blt)
					  (wait-until (thunk (unbox spread-signal)))
					  (bullet-clrflags blt (bltflags uncancelable))
					  (wait (* 10 layer))
					  (linear-step-accelerate (fl+ facing pi) 0.0 0.02 4.5 blt)
					  (linear-step-forever (fl+ facing pi) 4.5 task blt)))
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
  (ease-to ease-in-out-quad (enm-x enm) (fl+ 80.0 (enm-y enm)) 60 enm)
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
				  (facing-player (bullet-x blt) (bullet-y blt))
				  0.0 0.08 3.0 task blt))))))
	  task
	  (thunk (not (unbox dead-signalbox)))))
  (enm-superarmor-set! enm 80)
  (raylib:play-sound (sebundle-opshow sounds))
  (ease-to ease-in-out-quad (enm-x enm) (fl+ 80.0 (enm-y enm)) 60 enm)
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
	(define start-ang-to (facing-player (enm-x enm) (enm-y enm)))
	(raylib:play-sound (sebundle-laser sounds))
	(spawn-laser 'fixed-laser-blue
				 (enm-x enm) (enm-y enm)
				 start-ang-to
				 (fx2fl +playfield-height+)
				 5.0 20 delay
				 (λ (task blt)
				   (define ang-to (facing-player (bullet-x blt) (bullet-y blt)))
				   (let ([turn-dir
						  (if (fl< ang-to start-ang-to) -1.0 1.0)])
					 (interval-loop-while 15 (not (stop-pred))
					   (bullet-facing-set!
						blt
						(fl+ (bullet-facing blt) (fl* turn-dir (torad 4.0))))))))
	(spawn-subtask "sub"
	  (λ (task)
		(wait delay)
		(interval-loop 30
		  (-> (fb)
			  (fbspeed 3.2)
			  (fbcount 20)
			  (fbabsolute-aim)
			  (fbang (fl+ (centered-roll game-rng 2.5) (todeg -hpi)) 9.0)
			  (fbshootenm enm 'small-star-cyan 5 #f))))
	  task
	  (thunk (not (stop-pred)))))
  (enm-superarmor-set! enm 90)
  (wait 10)
  (raylib:play-sound (sebundle-opshow sounds))
  (ease-to ease-in-out-quad (enm-x enm) (fl+ 80.0 (enm-y enm)) 60 enm)
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
					(enm-x doremi) (enm-y doremi) non2-familiar-health
					(curry non2-dodo-control dodo-dead-signalbox)
					default-drop
					(curry non2-familiar-on-death dodo-dead-signalbox))
				   (enm-addflags (enmflags aura-red)))]
		 [rere-dead-signalbox (box #f)]
		 [rere (-> (spawn-enemy
					'rere
					(enm-x hazuki) (enm-y hazuki) non2-familiar-health
					(curry non2-rere-control rere-dead-signalbox)
					default-drop
					(curry non2-familiar-on-death rere-dead-signalbox))
				   (enm-addflags (enmflags aura-magenta)))]
		 [mimi-dead-signalbox (box #f)]
		 [mimi (-> (spawn-enemy
					'mimi
					(enm-x aiko) (enm-y aiko) non2-familiar-health
					(curry non2-mimi-control mimi-dead-signalbox)
					default-drop
					(curry non2-familiar-on-death mimi-dead-signalbox))
				   (enm-addflags (enmflags aura-blue)))])
	(wait-while keep-running))
  (common-nonspell-postlude bossinfo)
  (group-sp2 task doremi hazuki aiko))

(define group-sp2-center-x 0.0)
(define group-sp2-center-y 224.0)
(define group-sp2-fairy-rotate-easing
  (bezier-cubic-easing 0.4 -0.3 0.6 1.3))

(define (group-sp2-fairy-wave-var0 this-ang enm)
  (define is-dodo (eq? (enm-type enm) (enmtype dodo)))
  (let ([orb #f])
	(do [(j 0 (add1 j))]
		[(= j 8)]
	  (raylib:play-sound (sebundle-shoot0 sounds))
	  (-> (fb)
		  (fbcount 3)
		  (fbabsolute-aim)
		  (fbang (fl+ (todeg this-ang) 180.0) 60.0)
		  (fbspeed 3.25)
		  (fbshoot (enm-x enm) (enm-y enm)
			(λ (row col speed facing)
			  (-> (spawn-bullet
				   (vnth '#(arrowhead-white
							arrowhead-red arrowhead-orange arrowhead-yellow
							arrowhead-green arrowhead-cyan
							arrowhead-blue arrowhead-magenta) j)
				   (enm-x enm) (enm-y enm) 5
				   (λ (task blt)
					 (linear-step-decelerate facing speed -0.04 blt)
					 (when (and is-dodo
								(not orb) (= col 1)) ;; first center blt to reach
					   (set! orb
							 (spawn-bullet
							  'bubble-red
							  group-sp2-center-x group-sp2-center-y 5
							  values)))
					 (wait 10)
					 (let ([nf (flatan (fl- group-sp2-center-y
											(bullet-y blt))
									   (fl- group-sp2-center-x
											(bullet-x blt)))])
					   (bullet-facing-set! blt nf)
					   (loop-until (fl< (distsq (bullet-x blt)
												(bullet-y blt)
												group-sp2-center-x
												group-sp2-center-y)
										225.0)
						 (linear-step nf speed blt)))
					 (cancel-bullet blt)))
				  (bullet-facing-set! facing)))))
	  (wait 10))
	(raylib:play-sound (sebundle-longcharge sounds))
	(wait 145)
	(when is-dodo
	  (cancel-bullet orb))
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
			  (fl+ ang (torad (fl+ 17.0 (fx2fl j)))))))))

(define (group-sp2-fairy-wave-var1 ang enm)
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
										ang 160.0)])
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
								 (when (fl< (distsq (bullet-x blt) (bullet-y blt)
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
										  group-sp2-center-x group-sp2-center-y
										  (vnth-mod
										   '#(big-star-white
											  big-star-red big-star-orange big-star-yellow
											  big-star-green big-star-cyan
											  big-star-blue big-star-magenta) i)
										  5
										  (sebundle-bell sounds))))))))
				(bullet-facing-set! (fl+ ang pi)))))))
	(wait 15)
	(when (< i 15)
	  (loop (add1 i) (mod (+ gap gap-winding) blts) (box #f)))))

(define (group-sp2-fairy-ctrl init-ang task enm)
  (define init-dist 160.0)
  (define spin-time 180)
  (define-values (ix iy)
	(dist-away group-sp2-center-x group-sp2-center-y init-ang init-dist))
  (define _ (ease-to values ix iy 60 enm))
  (wait 60)
  (let wave ([wavei 0]
			 [ang init-ang])
	(do [(i 0 (add1 i))]
		[(fx= i spin-time)]
	  (let*-values
		  ([(dang) (fl- (lerp 0.0 (torad 120.0)
							  (group-sp2-fairy-rotate-easing
							   (inexact (/ i (sub1 spin-time))))))]
		   [(this-ang) (fl+ ang dang)]
		   [(x y)
			(dist-away group-sp2-center-x group-sp2-center-y this-ang init-dist)])
		(enm-x-set! enm x)
		(enm-y-set! enm y))
	  (yield))
	(let ([this-ang (fl- ang (torad 120.0))])
	  ;; consider randomizing (though that'll be more work since the three fairies
	  ;; need to independently roll the same result)
	  (case (mod wavei 2)
		[(0) (group-sp2-fairy-wave-var0 this-ang enm)]
		[(1) (group-sp2-fairy-wave-var1 this-ang enm)])
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
	   (enm-x enm) (enm-y enm) 1000 (curry group-sp2-fairy-ctrl init-ang))
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
  (let loop ([i 0]
			 [ang init-ang])
	(let-values ([(x y) (dist-away group-sp2-center-x group-sp2-center-y ang dist)])
	  (enm-x-set! enm x)
	  (enm-y-set! enm y))
	(let-values ([(q m) (div-and-mod i 5)])
	  (when (fxzero? m)
		(-> (spawn-bullet
			 'rice-white (enm-x enm) (enm-y enm) 5
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
					 (fbshootez (bullet-x blt) (bullet-y blt)
								(vnth-mod
								 '#(rice-red rice-orange rice-yellow
											 rice-green rice-blue rice-magenta) q)
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

(define (doremi-non2 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (set! current-chapter 24)
  (wait 90)
  (adjust-bars-non (bossinfo-healthbars bossinfo))
  (spawn-subtask "hazuki leave"
	(λ (_)
	  (ease-to ease-out-cubic
			   (if (flnegative? (enm-x hazuki)) -300.0 300.0)
			   (fl- (enm-y hazuki) 50.0) 60 hazuki)
	  (delete-enemy hazuki))
	task)
  (spawn-subtask "aiko leave"
	(λ (_)
	  (ease-to ease-out-cubic
			   (if (flnegative? (enm-x aiko)) -300.0 300.0)
			   (fl- (enm-y aiko) 50.0) 60 aiko)
	  (delete-enemy aiko))
	task)
  (ease-to ease-in-out-quad +middle-boss-x+ +middle-boss-y+ 60 doremi)
  (enm-clrflags doremi (enmflags nocollide))
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

(define (hazuki-non2-wisp-control ring1 ring2 task enm)
  (define start-frames frames)
  (spawn-subtask "control ring 1"
	(λ (task)
	  (loop-forever
	   (position-bullets-around-motion-facing
		(enm-x enm) (enm-y enm)
		(fl* 100.0 (flsin (fl* 1.0 (torad (fx2fl (- frames start-frames))))))
		(torad (fl/ (fx2fl (- frames start-frames)) 2.0))
		ring1)))
	task)
  (spawn-subtask "control ring 2"
	(λ (task)
	  (loop-forever
	   (position-bullets-around-motion-facing
		(enm-x enm) (enm-y enm)
		(fl* 60.0 (flsin (fl* 1.0 (torad (fx2fl (- frames start-frames))))))
		(torad (fl/ (fx2fl (- start-frames frames)) 2.0))
		ring2)))
	task)
  (ease-to values (enm-x enm) (fx2fl (+ +playfield-max-y+ 150)) 500 enm)
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
	  (if (fl< (distsq (bullet-x blt) (bullet-y blt) ex ey)
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
		 (set-box! wisp-dead-box (cons (enm-x enm) (enm-y enm)))
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
			(-> (cb)
				(cbcount 56)
				(cbabsolute-aim)
				(cbspeed 3.0)
				(cbshoot (enm-x hazuki) (enm-y hazuki)
				  (λ (layer in-layer speed facing)
					(-> (spawn-bullet 'rice-white (enm-x hazuki) (enm-y hazuki) 5
									  (curry linear-step-forever facing speed))
						(bullet-addflags (bltflags nocanceldrop))))))))
		task)
	  (spawn-subtask "decor"
		(λ (task)
		  (wait 180)
		  (interval-loop 20
			(-> (cb)
				(cbcount 56 2)
				(cbang 90.0)
				(cbabsolute-aim)
				(cbspeed 3.0 4.0)
				(cbshoot (enm-x hazuki) (enm-y hazuki)
				  (λ (layer in-layer speed facing)
					(unless (or (fx<= 0 in-layer 5)
								(fx<= 50 in-layer 55))
					  (-> (spawn-bullet 'rice-yellow (enm-x hazuki) (enm-y hazuki) 5
										(curry linear-step-forever facing speed))
						  (bullet-addflags (bltflags nocanceldrop)))))))))
		task)
	  (interval-loop 90
		(hazuki-non2-spawn-one
		 hazuki
		 (vrand '#(-70.0 0.0 70.0) game-rng))))
	task keep-running)
  (wait-while keep-running)
  (common-nonspell-postlude bossinfo)
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
			 (enm-x enm) (enm-y enm) 5
			 (λ (task blt)
			   (linear-step-decelerate facing1 4.0 -0.10 blt)
			   (wait 5)
			   (raylib:play-sound (sebundle-bell sounds))
			   (linear-step-accelerate-forever
				(flatan (fl- (fuzzed-player-y) (bullet-y blt))
						(fl- (fuzzed-player-x) (bullet-x blt)))
				0.0 0.05 (if killed-by-hazuki 4.5 3.0) task blt)))
			(spawn-bullet
			 'butterfly-magenta
			 (enm-x enm) (enm-y enm) 5
			 (λ (task blt)
			   (linear-step-decelerate facing2 2.0 -0.05 blt)
			   (wait 5)
			   (raylib:play-sound (sebundle-bell sounds))
			   (linear-step-accelerate-forever
				(flatan (fl- (fuzzed-player-y) (bullet-y blt))
						(fl- (fuzzed-player-x) (bullet-x blt)))
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
		(cbshoot (enm-x enm) (enm-y enm)
		  (λ (layer in-layer speed facing)
			(-> (spawn-bullet
				 (vnth '#(butterfly-orange butterfly-magenta butterfly-red) layer)
				 (enm-x enm) (enm-y enm) 5
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
	(define base-facing (facing-player (enm-x enm) (enm-y enm)))
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
						 (enmtype red-wisp) (enm-x hazuki) (enm-y hazuki) 350
						 (λ (task enm)
						   (enm-superarmor-set! enm 40)
						   (ease-to ease-in-out-quad x y 45 enm)
						   (loop-forever))
						 default-drop (curry hazuki-sp2-wisp-on-death
											 toptask
											 killed-by-hazuki-box hazuki))
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
							 (enm-x e) (enm-y e) 30
							 '((color . #x8b008ba0)
							   (r1 . 100.0)
							   (r2 . 20.0)))))
		 enms-and-boxes)
		(raylib:play-sound (sebundle-oldvwoopfast sounds))
		(for-each
		 (λ (pair)
		   (define e (car pair))
		   (define killed-by-hazuki-box (cdr pair))
		   (define facing (flatan (fl- (enm-y e) (enm-y hazuki))
								  (fl- (enm-x e) (enm-x hazuki))))
		   (when (fxpositive? (enm-health e))
			 (wait 20)
			 (spawn-bullet
				  'glow-orb-red (enm-x hazuki) (enm-y hazuki) 5
				  (λ (task blt)
					(define start-time frames)
					(spawn-subtask "trail"
					  (λ (task)
						(loop-forever
						 (spawn-bullet
						  'small-ball-magenta (bullet-x blt) (bullet-y blt) 8
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
								(bullet-x blt) (bullet-y blt)
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
  (wait 45)
  (ease-to ease-out-quad 0.0 179.0 45 hazuki)
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
  (aiko-non2 task hazuki))

(define (aiko-non2-laser-ctrl is-right aiko task blt)
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
					(enm-x aiko) (enm-y aiko)
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
				  (fl<= player-y (fl+ (enm-y aiko) 100.0)))
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
  (common-nonspell-postlude bossinfo)
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
	   (bullet-x-set! blt (enm-x aiko))
	   (bullet-y-set! blt (enm-y aiko))
	   (yield)
	   (loop state facing speed 0)]
	  [(move)
	   (when (flnegative? speed)
		 (loop 'stop 0.0 0.0 0))
	   (let ([ox (bullet-x blt)]
			 [oy (bullet-y blt)])
		 (linear-step facing speed blt)
		 (yield)

		 (when (> (bullet-y blt) (+ +playfield-max-y+ 10))
		   (damage-player)
		   (cancel-bullet blt #t)
		   ;; hacky imperative return to stop doing further processing
		   ;; we won't resume anymore because we canceled the bullet
		   (yield))

		 ;; collide with player
		 (let ([dx (fl- (bullet-x blt) player-x)]
			   [dy (fl- (bullet-y blt) player-y)]
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
					   (flatan (fl- (bullet-y blt) player-y)
							   (fl- (bullet-x blt) player-x))
					   (flatan (v2y last-player-movement-dir)
									 (v2x last-player-movement-dir)))
				   (fl* speed 1.01)
				   (fx1+ frames-moving))))

		 ;; collide with aiko
		 (when (fx> frames-moving 10) ;; prevent damaging on kick
		   (let ([dx (fl- (bullet-x blt) (enm-x aiko))]
				 [dy (fl- (bullet-y blt) (enm-y aiko))]
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
					 (flatan (fl- (bullet-y blt) (enm-y aiko))
							 (fl- (bullet-x blt) (enm-x aiko)))
					 (fl* speed 1.01)
					 (fx1+ frames-moving)))))

		 ;; bounce off walls
		 (let*-values ([(new-pos new-facing)
						(do-bounce-off (vec2 (bullet-x blt) (bullet-y blt))
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
   '((text . "Don't let Aiko score a goal!")
	 (size . 24.0)
	 (color . #x00ffffff)))
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
				(flatan (fl- target-y (bullet-y cur-ball))
						(fl- target-x (bullet-x cur-ball)))])
		  (set-box! msg-box (list 'move ball-facing 9.0)))
		(wait-until (thunk (or (fx> (fx- frames start-frames) 480)
							   (not (vector-index cur-ball live-bullets)))))
		(if (vector-index cur-ball live-bullets)
			(begin
			  (set-box! msg-box '(stop))
			  (enm-addflags aiko (enmflags nocollide))
			  (ease-to ease-in-out-quad
					   (bullet-x cur-ball) (bullet-y cur-ball) 60 aiko)
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

(define (group-sp3 task aiko)
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
  (set! current-chapter 30)
  (enm-extras-set! doremi bossinfo)
  (enm-extras-set! hazuki (blank-hazuki-bossinfo))
  (enm-redirect-damage-set! hazuki doremi)
  (enm-redirect-damage-set! aiko doremi)
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
  (enm-redirect-damage-set! hazuki doremi)
  (enm-redirect-damage-set! aiko doremi)
  (declare-spell doremi 11)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi)))))
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
  ;; todo formula
  (let ([clear-bonus 10000000])
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
