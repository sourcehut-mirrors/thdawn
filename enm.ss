;; Copyright (C) 2026 Vincent Lee; GPL-3.0-or-later
(define-enumeration enmtype
  (red-fairy green-fairy blue-fairy yellow-fairy
			 red-yinyang green-yinyang blue-yinyang magenta-yinyang
			 medium-red-fairy medium-blue-fairy
			 big-fairy boss-doremi boss-hazuki boss-aiko
			 red-wisp green-wisp blue-wisp yellow-wisp
			 dodo rere mimi dummy)
  make-enmtype-set)
(define-enumeration enmflag
  (invincible
   nocollide ;; cannot run into the player
   aura-red aura-green aura-blue aura-magenta ;; add decorative aura
   autocollect ;; drops are automatically collected
   )
  enmflags)
(define empty-enmflags (enmflags))
(define-record-type enm
  (fields
   type
   (mutable x)
   (mutable y)
   (mutable ox)
   (mutable oy)
   (mutable health)
   (mutable initial-health)
   ;; frame counter when this enemy was spawned
   time-spawned
   ;; if non-#f, redirect all damage to this enemy to this other enemy
   (mutable redirect-damage)
   ;; set to positive when damaged, decreases automatically every frame.
   ;; only use for visuals, because it's inaccurate when bosses have damage delegation
   (mutable damaged-recently)
   (mutable flags)
   ;; alist of (miscent type . count) to drop on death.
   (mutable drops)
   ;; optional function of enemy to be called when the enemy health reaches zero
   ;; if this returns #f, then the standard death logic is suppressed
   on-death
   ;; when positive, enemy has super armor. decreases automatically every frame.
   (mutable superarmor)
   ;; "x momentum" of the enemy.
   ;; every frame the enemy is moving right, this increments (resp. left/decrement).
   ;; if the enemy does not move on the X axis, this moves back towards zero.
   ;; Used to determine which sprite of the enemy to render for enemies with
   ;; different facing sprites.
   (mutable dx-render)
   ;; arbitrary scratchpad for custom data depending on type
   ;; - boss: contains a bossinfo instance
   (mutable extras))
  (sealed #t))
(alias ex enm-x)
(alias ey enm-y)

(define (enm-hasflag? enm flag)
  (enum-set-member? flag (enm-flags enm)))

(define (enm-addflags enm flags)
  (enm-flags-set! enm (enum-set-union (enm-flags enm) flags))
  enm)

(define (enm-clrflags enm flags)
  (enm-flags-set! enm (enum-set-difference (enm-flags enm) flags))
  enm)

;; for player hinting only, not for gameplay
(define (enm-lowhealth enm)
  (and (>= (enm-initial-health enm) 300)
	   (< (enm-health enm) (* (if (is-boss? enm) 0.1 0.2)
							  (enm-initial-health enm)))))

(define (is-boss? enm)
  (memq (enm-type enm) '(boss-doremi boss-hazuki boss-aiko)))
(define (find-bosses)
  (values
   (vector-find
	(λ (e) (and e (eq? 'boss-doremi (enm-type e)))) live-enm)
   (vector-find
	(λ (e) (and e (eq? 'boss-hazuki (enm-type e)))) live-enm)
   (vector-find
	(λ (e) (and e (eq? 'boss-aiko (enm-type e)))) live-enm)))
(define (find-spellcaster)
  (vector-find
   (λ (e)
	 (and e (is-boss? e)
		  (bossinfo-active-spell-id (enm-extras e))))
   live-enm))

(define (enm-invincible? enm)
  (or (enm-hasflag? enm (enmflag invincible))
	  (and (fxpositive? bombing)
		   (or (and (is-boss? enm)
					(bossinfo-active-spell-id (enm-extras enm)))
			   (let ([delegate (enm-redirect-damage enm)])
				 (and delegate
					  (is-boss? delegate)
					  (bossinfo-active-spell-id (enm-extras delegate))))))))

(define (enm-collision-box enm)
  (case (enm-type enm)
	([red-fairy green-fairy blue-fairy yellow-fairy dodo rere mimi]
	 (values (fl- (ex enm) 8.0)
			 (fl- (ey enm) 8.0)
			 16.0 16.0))
	([boss-doremi boss-hazuki boss-aiko]
	 (values (fl- (ex enm) 24.0)
			 (fl- (ey enm) 24.0)
			 48.0 48.0))
	([medium-blue-fairy medium-red-fairy
						red-yinyang green-yinyang blue-yinyang magenta-yinyang]
	 (values (fl- (ex enm) 11.0)
			 (fl- (ey enm) 11.0)
			 22.0 22.0))
	([blue-wisp green-wisp red-wisp yellow-wisp]
	 (values (fl- (ex enm) 9.0)
			 (fl- (ey enm) 9.0)
			 18.0 18.0))
	([big-fairy]
	 (values (fl- (ex enm) 15.0)
			 (fl- (ey enm) 15.0)
			 30.0 30.0))
	([dummy] (values 0.0 0.0 0.0 0.0))))

(define (enm-hurtbox enm)
  (case (enm-type enm)
	([red-fairy green-fairy blue-fairy yellow-fairy dodo rere mimi]
	 (values (fl- (ex enm) 16.0)
			 (fl- (ey enm) 16.0)
			 32.0 32.0))
	([boss-doremi boss-hazuki boss-aiko]
	 (values (fl- (ex enm) 24.0)
			 (fl- (ey enm) 24.0)
			 48.0 48.0))
	([medium-blue-fairy medium-red-fairy
						red-yinyang green-yinyang blue-yinyang magenta-yinyang]
	 (values (fl- (ex enm) 16.0)
			 (fl- (ey enm) 16.0)
			 32.0 32.0))
	([blue-wisp green-wisp red-wisp yellow-wisp]
	 (values (fl- (ex enm) 14.0)
			 (fl- (ey enm) 14.0)
			 28.0 28.0))
	([big-fairy]
	 (values (fl- (ex enm) 22.0)
			 (fl- (ey enm) 22.0)
			 44.0 44.0))
	([dummy] (values 0.0 0.0 0.0 0.0))))

(define (enm-tint enm)
  (define is-familiar (memq (enm-type enm) '(dodo rere mimi)))
  (cond
   [(and (not (or is-familiar (is-boss? enm)))
		 (enm-invincible? enm)
		 (< (fxmod frames 4) 2))
	invincible-flash]
   [(and (or is-familiar (is-boss? enm))
		 (enm-invincible? enm))
	#xffffff80]
   [(and (fxpositive? (enm-damaged-recently enm))
		 (< (fxmod frames 4) 2))
	(let ([target (or (enm-redirect-damage enm)
					  enm)])
	  (if (enm-lowhealth target) crit-damage-flash damage-flash))]
   [else -1]))

(define (draw-boss textures enm render-x render-y)
  (define bossinfo (enm-extras enm))
  (define lazy-t (-> (- frames (bossinfo-start-move-frame bossinfo))
					 (/ +spellcircle-context+)
					 (clamp 0 1)
					 (ease-in-out-quad)))
  (define lazy-render-x (+ +playfield-render-offset-x+
						   (lerp
							(bossinfo-start-move-x bossinfo)
							(ex enm)
							lazy-t)))
  (define lazy-render-y (+ +playfield-render-offset-y+
						   (lerp
							(bossinfo-start-move-y bossinfo)
							(ey enm)
							lazy-t)))
  ;; add the spawn timestamp to seed the rotation so multiple bosses don't
  ;; render their aura the exact same way
  (let* ([t (fx+ frames (enm-time-spawned enm))]
		 [radius (fl+ 90.0 (fl* 10.0 (flsin (/ t 12.0))))])
	(draw-sprite-pro-with-rotation
	 textures 'magicircle
	 (flmod (* t 3.0) 360.0)
	 ;; idk why I don't subtract the radius here but it works so :shrug:
	 (make-rectangle render-x render-y
					 (fl* 2.0 radius) (fl* 2.0 radius))
	 #xffffffff))

  ;; Okay, so raylib's draw-ring uses the ENTIRE shapes-texture for
  ;; every segment of the circle which is...not what we want.
  ;; However, I tried implementing ring drawing myself in the previous commit
  ;; and for some reason transparency is not working for it even though
  ;; my code looks exactly like what Raylib would do...
  ;; The compromise is to use Raylib's DrawRing, but only for a small sector (i.e. segments=1).
  ;; while having a loop on the Scheme side to continuously update the shape rect
  ;; TODO: Find a permanent solution for this clowntown
  (when (bossinfo-active-spell-id bossinfo)
	(let* ([boss-tex (txbundle-boss-flip textures)]
		   [save-tex (raylib:get-shapes-texture)]
		   [save-rect (raylib:get-shapes-texture-rectangle)]
		   [elapsed-frames (bossinfo-elapsed-frames bossinfo)])
	  (let* ([progress-of-90 (/ elapsed-frames 90.0)]
			 [inner-ring-radius (if (fx< elapsed-frames 90)
									(lerp 1.0 98.0 (ease-out-circ progress-of-90))
									(lerp
									 98.0 10.0
									 (/ (fx- elapsed-frames 90)
										(fx- (bossinfo-total-timer bossinfo) 90))))]
			 [inner-ring-gap (if (fx< elapsed-frames 90)
								 (fl+ 12.0 (lerp 20.0 0.0
												 (ease-in-quart progress-of-90)))
								 12.0)]
			 [inner-ring-brightness (if (fx< elapsed-frames 90)
										(eround (lerp 160 235 progress-of-90))
										235)])
		(raylib:with-matrix
		 (raylib:translatef lazy-render-x lazy-render-y 0.0)
		 (raylib:rotatef (flmod (* frames -4.0) 360.0) 0.0 0.0 1.0)
		 (do [(u 0.0 (fl+ u 4.0)) ;; 128/32
			  (ang 0.0 (fl+ ang 11.25))] ;; 360/32
			 [(fl>= u 128.0)]
		   (raylib:set-shapes-texture boss-tex (make-rectangle u 48.0 4.0 16.0))
		   (raylib:draw-ring 0.0 0.0 inner-ring-radius (fl+ inner-ring-radius
															inner-ring-gap)
							 ang (fl+ ang 11.25) 1
							 (fxlogior #xffffff00 inner-ring-brightness)))))

	  (let* ([outer-ring-radius
			  (cond
			   [(fx< elapsed-frames 50)
				(lerp 0.0 310.0 (ease-out-cubic (/ elapsed-frames 50.0)))]
			   [(fx< elapsed-frames 80)
				(lerp 310.0 108.0
					  (ease-in-quad (/ (fx- elapsed-frames 50) 30.0)))]
			   [else (lerp 108.0 20.0
						   (/ (fx- elapsed-frames 80)
							  (fx- (bossinfo-total-timer bossinfo) 80)))])])
		(raylib:with-matrix
		 (raylib:translatef lazy-render-x lazy-render-y 0.0)
		 (raylib:rotatef (flmod (* frames 5.2) 360.0) 0.0 0.0 1.0)
		 (do [(u 0.0 (fl+ u 4.0))
			  (ang 0.0 (fl+ ang 11.25))]
			 [(fl>= u 128.0)]
		   (raylib:set-shapes-texture boss-tex (make-rectangle u 80.0 4.0 16.0))
		   (raylib:draw-ring 0.0 0.0 outer-ring-radius (fl+ outer-ring-radius 12.0)
							 ang (fl+ ang 11.25) 1 #xffffffeb))))

	  (raylib:set-shapes-texture save-tex save-rect)))
  ;; TODO actual sprites lol
  (draw-sprite textures 'yellow-fairy2 render-x render-y (enm-tint enm)))

(define (draw-enemies textures)
  (define (each enm)
	(when enm
	  (let ([render-x (+ (ex enm) +playfield-render-offset-x+)]
			[render-y (+ (ey enm) +playfield-render-offset-y+)]
			[dx (enm-dx-render enm)]
			[type (enm-type enm)]
			[tint (enm-tint enm)])
		(case type
		  ([boss-doremi boss-hazuki boss-aiko]
		   (draw-boss textures enm render-x render-y))
		  ([red-yinyang green-yinyang blue-yinyang magenta-yinyang]
		   (draw-sprite textures type render-x render-y -1)
		   (let ([outer-sprite
				  (case type
					([red-yinyang] 'red-yinyang-outer)
					([green-yinyang] 'green-yinyang-outer)
					([blue-yinyang] 'blue-yinyang-outer)
					([magenta-yinyang] 'magenta-yinyang-outer))])
			 (draw-sprite-with-scale-rotation
			  textures
			  outer-sprite
			  (fx2fl (fxmod (fx* frames -4) 360))
			  1.4 render-x render-y tint)
			 (draw-sprite-with-rotation
			  textures
			  outer-sprite
			  (fx2fl (fxmod (fx* frames 8) 360))
			  render-x render-y tint)))
		  ([red-wisp blue-wisp green-wisp yellow-wisp]
		   (let* ([sprites
				   (case type
					 ([red-wisp] '#(red-wisp0 red-wisp1 red-wisp2
											  red-wisp3 red-wisp4 red-wisp5
											  red-wisp6 red-wisp7))
					 ([blue-wisp] '#(blue-wisp0 blue-wisp1 blue-wisp2
												blue-wisp3 blue-wisp4 blue-wisp5
												blue-wisp6 blue-wisp7))
					 ([green-wisp] '#(green-wisp0 green-wisp1 green-wisp2
												  green-wisp3 green-wisp4 green-wisp5
												  green-wisp6 green-wisp7))
					 ([yellow-wisp] '#(yellow-wisp0 yellow-wisp1 yellow-wisp2
													yellow-wisp3 yellow-wisp4 yellow-wisp5
													yellow-wisp6 yellow-wisp7)))]
				  [t (fx+ frames (enm-time-spawned enm))]
				  [sprite (vnth-mod sprites (fx/ t 5))]
				  [aura-sprite
				   (cond
					[(enm-hasflag? enm (enmflag aura-red))
					 'aura-red]
					[(enm-hasflag? enm (enmflag aura-green))
					 'aura-green]
					[(enm-hasflag? enm (enmflag aura-blue))
					 'aura-blue]
					[(enm-hasflag? enm (enmflag aura-magenta))
					 'aura-magenta]
					[else #f])])
			 (when aura-sprite
			   (draw-sprite-with-scale-rotation
				textures aura-sprite
				(fxmod (fx* 2 t) 360)
				(fl+ 1.2 0.2 (fl* 0.2 (flsin (/ t 10.0))))
				render-x render-y -1))
			 (draw-sprite textures sprite render-x render-y tint)))
		  ([yellow-fairy red-fairy green-fairy blue-fairy
						 medium-red-fairy medium-blue-fairy big-fairy
						 dodo rere mimi]
		   (let ([t (fx+ frames (enm-time-spawned enm))]
				 [aura-sprite
				  (cond
				   [(enm-hasflag? enm (enmflag aura-red))
					'aura-red]
				   [(enm-hasflag? enm (enmflag aura-green))
					'aura-green]
				   [(enm-hasflag? enm (enmflag aura-blue))
					'aura-blue]
				   [(enm-hasflag? enm (enmflag aura-magenta))
					'aura-magenta]
				   [else #f])])
			 (when aura-sprite
			   (draw-sprite-with-scale-rotation
				textures aura-sprite
				(fxmod (fx* 2 t) 360)
				(fl+ 1.2 0.2 (fl* 0.2 (flsin (/ t 10.0))))
				render-x render-y -1)))
		   (cond
			[(fl< (abs dx) 5.0)
			 (let* ([fwd-sprites
					 (case type
					   ([yellow-fairy rere]
						'#(yellow-fairy1 yellow-fairy2
										 yellow-fairy3 yellow-fairy4))
					   ([red-fairy dodo]
						'#(red-fairy1 red-fairy2 red-fairy3 red-fairy4))
					   ([green-fairy] '#(green-fairy1 green-fairy2 green-fairy3 green-fairy4))
					   ([blue-fairy mimi]
						'#(blue-fairy1 blue-fairy2 blue-fairy3 blue-fairy4))
					   ([medium-blue-fairy] '#(medium-blue-fairy0 medium-blue-fairy1 medium-blue-fairy2 medium-blue-fairy3))
					   ([medium-red-fairy] '#(medium-red-fairy0 medium-red-fairy1 medium-red-fairy2 medium-red-fairy3))
					   ([big-fairy] '#(big-fairy0 big-fairy1 big-fairy2 big-fairy3)))]
					[sprite (vnth-mod fwd-sprites (fx/ frames 5))])
			   (draw-sprite textures sprite render-x render-y tint))]
			[(fl< (abs dx) 10.0)
			 (let* ([transition-sprites
					 (case type
					   ([yellow-fairy rere] '#(yellow-fairy5))
					   ([red-fairy dodo] '#(red-fairy5))
					   ([green-fairy] '#(green-fairy5))
					   ([blue-fairy mimi] '#(blue-fairy5))
					   ([medium-blue-fairy] '#(medium-blue-fairy4 medium-blue-fairy5 medium-blue-fairy6 medium-blue-fairy7))
					   ([medium-red-fairy] '#(medium-red-fairy4 medium-red-fairy5 medium-red-fairy6 medium-red-fairy7))
					   ([big-fairy] '#(big-fairy4 big-fairy5 big-fairy6 big-fairy7)))]
					[sprite (vnth-mod transition-sprites (fx/ frames 7))])
			   (if (flnegative? dx)
				   (draw-sprite-mirror-x textures sprite render-x render-y tint)
				   (draw-sprite textures sprite render-x render-y tint)))]
			[else
			 (let* ([side-sprites
					 (case type
					   ([yellow-fairy rere]
						'#(yellow-fairy6 yellow-fairy7 yellow-fairy8
										 yellow-fairy9 yellow-fairy10 yellow-fairy11))
					   ([red-fairy dodo]
						'#(red-fairy6 red-fairy7 red-fairy8
									  red-fairy9 red-fairy10 red-fairy11))
					   ([green-fairy]
						'#(green-fairy6 green-fairy7 green-fairy8
										green-fairy9 green-fairy10 green-fairy11))
					   ([blue-fairy mimi]
						'#(blue-fairy6 blue-fairy7 blue-fairy8
									   blue-fairy9 blue-fairy10 blue-fairy11))
					   ([medium-blue-fairy] '#(medium-blue-fairy8 medium-blue-fairy9 medium-blue-fairy10 medium-blue-fairy11))
					   ([medium-red-fairy] '#(medium-red-fairy8 medium-red-fairy9 medium-red-fairy10 medium-red-fairy11))
					   ([big-fairy] '#(big-fairy8 big-fairy9 big-fairy10 big-fairy11)))]
					[sprite (vnth-mod side-sprites (fx/ frames 7))])
			   (if (flnegative? dx)
				   (draw-sprite-mirror-x textures sprite render-x render-y tint)
				   (draw-sprite textures sprite render-x render-y tint)))]))
		  ([dummy]
		   (let ([t (fx+ frames (enm-time-spawned enm))]
				 [aura-sprite
				   (cond
					[(enm-hasflag? enm (enmflag aura-red))
					 'aura-red]
					[(enm-hasflag? enm (enmflag aura-green))
					 'aura-green]
					[(enm-hasflag? enm (enmflag aura-blue))
					 'aura-blue]
					[(enm-hasflag? enm (enmflag aura-magenta))
					 'aura-magenta]
					[else #f])])
			 (when aura-sprite
			   (draw-sprite-with-scale-rotation
				textures aura-sprite
				(fxmod (fx* 2 t) 360)
				(fl+ 1.2 0.2 (fl* 0.2 (flsin (/ t 10.0))))
				render-x render-y -1)))))
		(when show-hitboxes
		  (let-values ([(x y w h) (enm-hurtbox enm)])
			(raylib:draw-rectangle-rec
			 (+ x +playfield-render-offset-x+)
			 (+ y +playfield-render-offset-y+) w h green))
		  (unless (enm-hasflag? enm (enmflag nocollide))
			(let-values ([(x y w h) (enm-collision-box enm)])
			  (raylib:draw-rectangle-rec
			   (+ x +playfield-render-offset-x+)
			   (+ y +playfield-render-offset-y+) w h red)))))))
  (vector-for-each each live-enm))
