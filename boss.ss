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
				 (constantly #f)))
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
				 (constantly #f)))
  (define bossinfo (blank-aiko-bossinfo))
  (define keep-running?
	(thunk
	 (and (positive? (bossinfo-remaining-timer bossinfo))
		  (positive? (enm-health aiko)))))
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
	keep-running? task)
  (wait-while keep-running?)
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
  (ease-to ease-in-out-quad +middle-boss-x+ +middle-boss-y+ 45 aiko)
  (wait 55)
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
	  (thunk (not (unbox dead-signalbox)))
	  task))
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
	  (thunk (not (unbox dead-signalbox)))
	  task))
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
	  (thunk (not (stop-pred)))
	  task))
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
  (bossinfo-redirect-damage-set!
   (enm-extras hazuki) doremi)
  (bossinfo-redirect-damage-set!
   (enm-extras aiko) doremi)
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
				 (constantly #f)))
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

(define (hazuki-sp2-wisp-on-death killed-by-hazuki-box hazuki enm)
  (define killed-by-hazuki (unbox killed-by-hazuki-box))
  ;; don't run when being cleared by the attack ending
  (when (fxpositive? (enm-health hazuki))
	(-> (cb)
		(cbabsolute-aim)
		(cbang (fl* 360.0 (roll game-rng)) 50.0)
		(cbcount 24 (if killed-by-hazuki 3 2))
		(cbspeed 2.0 2.5)
		(cbshoot (enm-x enm) (enm-y enm)
		  (λ (layer in-layer speed facing)
			(-> (spawn-bullet
				 (vnth-mod '#(butterfly-orange butterfly-magenta butterfly-red) in-layer)
				 (enm-x enm) (enm-y enm) 5
				 (λ (task blt)
				   (let loop ([facing facing]
							  [i 0])
					 (bullet-facing-set! blt facing)
					 (if (< i 100)
						 (begin
						   (linear-step facing speed blt)
						   (yield)
						   (loop (fl+ facing (torad 1.5))
								 (add1 i)))
						 (linear-step-forever facing speed task blt)))))
				(bullet-facing-set! facing)))))
	(-> (cb)
		(cbcount 20)
		(cbspeed 3.5)
		(cbshootenm enm 'small-ball-white 5
					(sebundle-bell sounds)))
	(unless killed-by-hazuki
	  (damage-enemy hazuki 800 #t #t))))

(define (hazuki-sp2-wave all-waves-clean-box toptask hazuki)
  (define start-time frames)
  (define (get-point i)
	(define ang (fl+ (fl* hpi (fx2fl i))
					 (torad -20.0)
					 (torad (centered-roll game-rng 10.0))))
	(define dist
	  (case i
;		[(1 2) (fl+ 80.0 (fl* 120.0 (roll game-rng)))]
		[else (fl+ 80.0 (fl* 100.0 (roll game-rng)))]))
	(dist-away hazuki ang dist))
  (define enms-and-boxes
	(let loop ([acc '()]
			   [i 0])
	  (if (= i 4)
		  (reverse! acc)
		  (let-values ([(x y) (get-point i)]
					   [(dont-damage-hazuki-box) (box #f)])
			(wait 20)
			(raylib:play-sound (sebundle-opshow sounds))
			(loop 
			 (cons
			  (cons (-> (spawn-enemy
						 (enmtype red-wisp) (enm-x hazuki) (enm-y hazuki) 350
						 (λ (task enm)
						   (ease-to ease-in-out-quad x y 45 enm)
						   (loop-forever))
						 default-drop (curry hazuki-sp2-wisp-on-death
											 dont-damage-hazuki-box hazuki))
						(enm-addflags (enmflags aura-red)))
					dont-damage-hazuki-box)
			  acc)
			 (add1 i))))))
  (define (all-dead)
	(for-all (λ (pair) (fxnonpositive? (enm-health (car pair))))
			 enms-and-boxes))
  (define killer-task
	(spawn-subtask "kill"
	  (λ (task)
		(wait-until (thunk (fx>= (fx- frames start-time) 300)))
		(for-each
		 (λ (pair)
		   (define e (car pair))
		   (wait 20)
		   (when (fxpositive? (enm-health e))
			 (raylib:play-sound (sebundle-shortcharge sounds))
			 (spawn-particle (particletype circle-hint-opaque)
							 (enm-x e) (enm-y e) 30
							 '((color . #x8b008ba0)
							   (r1 . 100.0)
							   (r2 . 20.0)))))
		 enms-and-boxes)
		(raylib:play-sound (sebundle-longcharge sounds))
		(for-each
		 (λ (pair)
		   (define e (car pair))
		   (define dont-damage-hazuki-box (cdr pair))
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
					  (constantly #t) task)
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
							  (dotimes 40
								(spawn-bullet
								 'pellet-blue
								 (fl+ x (fl/ w 2.0)) (fl+ y (fl/ h 2.0)) 5
								 (λ (task blt)
								   (define facing (fl* tau (roll game-rng)))
								   (define speed (roll-flrange
												  game-rng 1.8 3.0))
								   (linear-step-decelerate-to
									facing speed -0.05 0.9 blt)
								   (linear-step-forever facing 0.9 task blt)))
								(yield)))
							(constantly #t)
							toptask)
						  (set-box! all-waves-clean-box #f)
						  (set-box! dont-damage-hazuki-box #t)
						  (kill-enemy e)
						  (cancel-bullet blt))))))))
		 enms-and-boxes))
	  (thunk (not (all-dead)))
	  toptask))
  (wait-until (thunk (task-dead killer-task)))
  )

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
	keep-running task)
  (wait-while keep-running)
  ;; Extra bonus if all waves were cleared without a wisp being killed by hazuki
  (when (unbox all-waves-clean)
	(spawn-particle
	 (particletype spellbonus)
	 0.0 100.0 180
	 (format "EX Bonus!! ~:d" 5000000))
	(set! current-score (+ current-score 5000000)))
  (common-spell-postlude bossinfo hazuki)
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


(define (aiko-sp2-ball-ctrl msg-box aiko task blt)
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
				 ;; Extra bonus if the ball was the killing blow
				 (when (and (fxpositive? old-health)
							(fxnonpositive? (enm-health aiko)))
				   (spawn-particle
					(particletype spellbonus)
					0.0 100.0 180
					(format "EX Bonus!! ~:d" 5000000))
				   (set! current-score (+ current-score 5000000))))
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
  (define (keep-running?)
	(and (fxpositive? (bossinfo-remaining-timer bossinfo))
		 (fxpositive? (enm-health aiko))))
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
		(when (fl< player-y 300.0)
		  (-> (cb)
			  (cbcount 36)
			  (cbspeed 2.0)
			  (cbshootenm aiko 'kunai-magenta 5 #f)))))
	keep-running? task)
  (spawn-subtask "main"
	(λ (task)
	  (define (spawn-ball)
		(parameterize ([ovr-uncancelable #t]
					   [ovr-noclip #t])
		  (spawn-bullet 'yinyang-green 0.0 200.0 5
						(curry aiko-sp2-ball-ctrl msg-box aiko))))
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
	keep-running? task)
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
