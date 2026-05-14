;; Copyright (C) 2026 Vincent Lee; GPL-3.0-or-later
(define (get-next-bullet-id)
  (define res (stage-ctx-next-bullet-id current-stage-ctx))
  (stage-ctx-next-bullet-id-set! current-stage-ctx (fx1+ res))
  res)

(define-record-type bullet
  (fields
   id ;; globally incrementing number. Used to sort the bullets before rendering.
   type
   (mutable x)
   (mutable y)
   ;; radians. This is used ONLY for rendering, not for movement!
   ;; Bullet control functions should initialize this,
   ;; and also update it every frame if it's supposed to follow along with movement
   (mutable facing)
   (mutable grazed)
   ;; how many frames we've been alive. If < 0, then bullet is in "prespawn"
   ;; and does not participate in gameplay, only renders a preimg sprite
   (mutable livetime)
   initial-livetime
   (mutable flags)))
(alias bx bullet-x)
(alias by bullet-y)

(define-record-type laser
  (parent bullet)
  (fields
   (mutable length)
   radius ;; aka half-thickness
   despawn-time ;; how long the laser despawn animation takes
   (mutable last-grazed-at)
   (mutable start-despawning-at))
  (sealed #t))

(define-record-type blttype
  (fields
   id
   family
   preimg-sprite
   ;; NB: lasers have custom preimg rendering, and their hit radius is adjustable
   ;; per-laser, so these three fields are unused for lasers.
   preimg-begin-size preimg-end-size
   hit-radius)
  (sealed #t))

;; cached sprites: symbol ht type -> vector of the sprite ids
;; to avoid repetitive symbol munging/allocation at render time
(define bullet-types
  (let* ([ret (make-hashtable symbol-hash eq?)]
		 [preimg-sprite-mapping
		  (map (λ (color)
				 (cons color (string->symbol (string-append "preimg-" color))))
			   basic-colors-str)]
		 [make-family
		  (λ (family colors hit-radius)
			(for-each (λ (color)
						(define type
						  (string->symbol (string-append
										   (symbol->string family)
										   "-"
										   color)))
						(define preimg-sprite
						  (let ([entry (assq color preimg-sprite-mapping)])
							(if entry (cdr entry) 'preimg-white)))
						(symbol-hashtable-set!
						 ret
						 type
						 (make-blttype type family preimg-sprite
									   20.0 2.0
									   hit-radius)))
					  colors))])
	(make-family 'small-star basic-colors-str 3.7)
	(make-family 'big-star basic-colors-str 5.5)
	(make-family 'rice basic-colors-str 2.0)
	(make-family 'pellet basic-colors-str 2.0)
	(make-family 'butterfly basic-colors-str 3.7)
	(make-family 'ellipse basic-colors-str 4.0)
	(make-family 'arrowhead basic-colors-str 3.0)
	(make-family 'amulet basic-colors-str 3.1)
	(make-family 'glow-ball basic-colors-str 3.0)
	(make-family 'small-ball basic-colors-str 3.0)
	(make-family 'medium-ball basic-colors-str 9.0)
	(make-family 'ice-shard basic-colors-str 2.5)
	(make-family 'fixed-laser basic-colors-str 0.0)
	(make-family 'rest basic-colors-str 3.0)
	(make-family 'music basic-colors-str 3.0)
	(make-family 'knife basic-colors-str 4.0)
	(make-family 'bacteria basic-colors-str 2.0)
	(make-family 'kunai basic-colors-str 2.5)
	(make-family 'droplet basic-colors-str 2.0)
	(make-family 'heart basic-colors-str 6.0)
	(make-family 'arrow basic-colors-str 3.0)
	(make-family 'glow-orb basic-colors-str 9.0)
	(make-family 'fireball basic-colors-str 5.5)
	(make-family 'yinyang '("red" "green" "blue" "magenta") 8.0)
	(for-each (λ (color)
				(define type
				  (string->symbol (string-append "bubble-" color)))
				(symbol-hashtable-set!
				 ret
				 type
				 (make-blttype type 'bubble type 2.0 32.0 16.0)))
			  basic-colors-str)
	(symbol-hashtable-set! ret 'placeholder
						   (make-blttype 'placeholder 'placeholder 'preimg-white
										 0.0 0.0 0.0))
	ret))
(define-values (cached-music-sprites cached-fireball-sprites)
  (let ([music (make-hashtable symbol-hash eq?)]
		[fireball (make-hashtable symbol-hash eq?)])
	(vector-for-each
	 (λ (pair)
	   (define id (car pair))
	   (when (eq? 'music (blttype-family (cdr pair)))
		 (symbol-hashtable-set!
		  music id
		  (list->vector
		   (map (λ (i)
				  (string->symbol
				   (string-append (symbol->string id) (number->string i))))
				(iota 3)))))
	   (when (eq? 'fireball (blttype-family (cdr pair)))
		 (symbol-hashtable-set!
		  fireball id
		  (list->vector
		   (map (λ (i)
				  (string->symbol
				   (string-append (symbol->string id) (number->string i))))
				(iota 4))))))
	 (hashtable-cells bullet-types))
	(values music fireball)))

(define (bullet-active? blt)
  ;; whether the bullet participates in gameplay
  (and (fxnonnegative? (bullet-livetime blt))
	   (or (not (laser? blt))
		   (not (laser-start-despawning-at blt)))))

(define (bullet-hasflag? blt flag)
  (enum-set-member? flag (bullet-flags blt)))

(define (bullet-addflags blt flags)
  (bullet-flags-set! blt (enum-set-union (bullet-flags blt) flags))
  blt)

(define (bullet-clrflags blt flags)
  (bullet-flags-set! blt (enum-set-difference (bullet-flags blt) flags))
  blt)

(define (spawn-bullet type x y delay control-function)
  (let ([idx (vector-index #f live-bullets)])
	(unless idx
	  (error 'spawn-bullet "No more open bullet slots"))
	(let ([blt (make-bullet (get-next-bullet-id)
							type x y 0.0 #f (- delay) (- delay)
							empty-bltflags)]
		  [sealed (let ([s (seal-distance)])
					(and s (fl<= (distsq x y player-x player-y) (* s s))))])
	  (when (ovr-uncancelable)
		(bullet-addflags blt (bltflags uncancelable)))
	  (when (ovr-noclip)
		(bullet-addflags blt (bltflags noclip)))
	  (when (ovr-nocanceldrop)
		(bullet-addflags blt (bltflags nocanceldrop)))
	  (unless sealed
		(vector-set! live-bullets idx blt)
		(spawn-task "bullet"
		  (λ (task)
			(do [(i 0 (fx1+ i))]
				[(fx> i delay)]
			  (bullet-livetime-set! blt (fx1+ (bullet-livetime blt)))
			  (yield))
			(control-function task blt))
		  (thunk (eq? blt (vnth live-bullets idx)))))
	  blt)))

(define (spawn-laser type x y facing length radius despawn-time
					 delay control-function)
  (let ([idx (vector-index #f live-bullets)])
	(unless idx
	  (error 'spawn-bullet "No more open bullet slots"))
	(let ([blt (make-laser (get-next-bullet-id)
						   type x y facing #f (- delay) (- delay)
						   empty-bltflags
						   length radius despawn-time -1 #f)])
	  (vector-set! live-bullets idx blt)
	  (when (ovr-uncancelable)
		(bullet-addflags blt (bltflags uncancelable)))
	  (spawn-task "laser"
				  (λ (task)
					(do [(i 0 (fx1+ i))]
						[(fx> i delay)]
					  (bullet-livetime-set! blt (fx1+ (bullet-livetime blt)))
					  (yield))
					(control-function task blt)
					(laser-start-despawning-at-set! blt frames)
					(wait despawn-time)
					(delete-bullet blt))
				  (thunk (eq? blt (vnth live-bullets idx))))
	  blt)))

(define (delete-bullet bullet)
  (let ([idx (vector-index bullet live-bullets)])
	(when idx
	  (vector-set! live-bullets idx #f))))

(define cancel-bullet
  (case-lambda
	([bullet] (cancel-bullet bullet #f))
	([bullet force]
	 (and (or force (not (bullet-hasflag? bullet (bltflag uncancelable))))
		  (or force (bullet-active? bullet))
		  (spawn-particle
		   (particletype cancel)
		   (bx bullet) (by bullet)
		   23 #f)
		  (delete-bullet bullet)))))

(define cancel-bullet-with-drop
  (case-lambda
	([bullet drop] (cancel-bullet-with-drop bullet drop #f))
	([bullet drop force]
	 (when (and (cancel-bullet bullet force)
				(not (bullet-hasflag? bullet (bltflag nocanceldrop))))
	   (spawn-drops-with-autocollect
		(list (cons drop 1))
		(bx bullet) (by bullet))))))

(define (cancel-all force)
  (vector-for-each
   (λ (blt)
	 (and blt (cancel-bullet-with-drop blt 'small-piv force)))
   live-bullets))

(define (despawn-out-of-bound-bullet bullet)
  (when bullet
	(let ([x (bx bullet)]
		  [y (by bullet)])
	  (when (and
			 (not (bullet-hasflag? bullet (bltflag noprune)))
			 (or (> x (+ +playfield-max-x+ +oob-bullet-despawn-fuzz+))
				 (< x (- +playfield-min-x+ +oob-bullet-despawn-fuzz+))
				 (< y (- +playfield-min-y+ +oob-bullet-despawn-fuzz+))
				 (> y (+ +playfield-max-y+ +oob-bullet-despawn-fuzz+))))
		(delete-bullet bullet)))))

(define (bullet-family type)
  (define bt (symbol-hashtable-ref bullet-types type #f))
  (blttype-family bt))

(define (bullet-hit-radius type)
  (define bt (symbol-hashtable-ref bullet-types type #f))
  (blttype-hit-radius bt))

(define (draw-lasers textures sorted-bullets)
  (define (each bullet)
	(when bullet
	  (let* ([render-x (+ (bx bullet) +playfield-render-offset-x+)]
			 [render-y (+ (by bullet) +playfield-render-offset-y+)]
			 [type (bullet-type bullet)]
			 [bt (symbol-hashtable-ref bullet-types type #f)]
			 [livetime (bullet-livetime bullet)])
		(case (bullet-family type)
		  ([fixed-laser]
		   (let* ([length (laser-length bullet)]
				  [full-radius (laser-radius bullet)]
				  [start-despawning-at (laser-start-despawning-at bullet)]
				  [radius (cond
						   [(fx<= livetime -10) 2.0]
						   [(fx<= livetime 0)
							(lerp 2.0 full-radius
								  (- 1 (/ livetime -10)))]
						   [start-despawning-at
							(lerp full-radius 0.0
								  (/ (- frames start-despawning-at)
									 (laser-despawn-time bullet)))]
						   [else full-radius])])
			 (draw-laser-sprite textures type render-x render-y
								length radius (bullet-facing bullet)
								(and (not (bullet-hasflag? bullet (bltflag noshine)))
									 (blttype-preimg-sprite bt)))))))))
  (vector-for-each each sorted-bullets))


(define (draw-bullets textures sorted-bullets)
  (define (each bullet)
	(when bullet
	  (let* ([render-x (fl+ (bx bullet) (fx2fl +playfield-render-offset-x+))]
			 [render-y (fl+ (by bullet) (fx2fl +playfield-render-offset-y+))]
			 [type (bullet-type bullet)]
			 [bt (symbol-hashtable-ref bullet-types type #f)]
			 [livetime (bullet-livetime bullet)])
		(if (and (not (eq? 'fixed-laser (bullet-family type)))
				 (fxnegative? livetime))
			(let* ([preimg-begin (blttype-preimg-begin-size bt)]
				   [preimg-end (blttype-preimg-end-size bt)]
				   ;; reversed because the factor is negative
				   [radius (lerp preimg-end preimg-begin
								 (fl/ (fx2fl livetime)
									  (fx2fl (bullet-initial-livetime bullet))))])
			  (draw-sprite-pro
			   textures (blttype-preimg-sprite bt)
			   (make-rectangle (fl- render-x radius) (fl- render-y radius)
							   (fl* 2.0 radius) (fl* 2.0 radius))
			   -1))
			(let ()
			  (case (bullet-family type)
				;; basic
				([pellet small-ball glow-ball medium-ball glow-orb]
				 (draw-sprite textures type render-x render-y #xffffffff))
				;; aimed in direction of movement
				([butterfly ellipse arrowhead amulet ice-shard rice
							rest knife bacteria kunai droplet heart arrow]
				 (draw-sprite-with-rotation textures type
											(todeg (bullet-facing bullet))
											render-x render-y -1))
				;; spinny
				([small-star big-star]
				 (draw-sprite-with-rotation textures type
											(fx2fl (fxmod (fx* frames 5) 360))
											render-x render-y -1))
				([bubble yinyang]
				 (draw-sprite-with-rotation textures type
											(fx2fl (fxmod (fx* frames 8) 360))
											render-x render-y -1))
				([music]
				 (let ([sprite
						(vnth-mod (symbol-hashtable-ref cached-music-sprites type #f)
								  (fx/ frames 10))])
				   (draw-sprite-with-rotation textures sprite 90.0
											  render-x render-y -1)))
				([fireball]
				 (let ([sprite
						(vnth-mod (symbol-hashtable-ref cached-fireball-sprites type #f)
								  (fx/ frames 7))])
				   (draw-sprite-with-rotation
					textures sprite
					(todeg (bullet-facing bullet))
					render-x render-y -1))))
			  (when show-hitboxes
				(raylib:draw-circle-v render-x render-y (bullet-hit-radius type)
									  red)))))))
  (vector-for-each each sorted-bullets))
