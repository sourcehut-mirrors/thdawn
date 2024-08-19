(import (add-prefix (raylib) raylib:)
		(coro) (geom))
(define key-space 32)
(define key-escape 256)
(define key-f3 292)
(define key-left-shift 340)
(define key-right 262)
(define key-left 263)
(define key-down 264)
(define key-up 265)
(define key-z 90)
(define pi 3.141592)

(define (ease-out-cubic x)
  (- 1 (expt (- 1 x) 3.0)))

(define-record-type sebundle
  (fields
   spellcapture spelldeclare
   longcharge shortcharge
   enmdie bossdie
   playerdie playershoot
   shoot0 shoot1 shoot2
   extend graze bell
   oldvwoopfast oldvwoopslow
   pause menuselect
   timeout timeoutwarn))
(define sounds #f)
(define (load-sfx)
  (define (lsfx file) (raylib:load-sound (string-append "assets/sfx/" file)))
  (set! sounds
		(make-sebundle
		 (lsfx "se_cardget.wav") (lsfx "se_cat00.wav")
		 (lsfx "se_ch00.wav") (lsfx "se_ch02.wav")
		 (lsfx "se_enep00.wav") (lsfx "se_enep01.wav")
		 (lsfx "se_pldead00.wav") (lsfx "se_plst00.wav")
		 (lsfx "se_tan00.wav") (lsfx "se_tan01.wav") (lsfx "se_tan02.wav")
		 (lsfx "se_extend.wav") (lsfx "se_graze.wav") (lsfx "se_kira00.wav")
		 (lsfx "se_power1.wav") (lsfx "se_power2.wav")
		 (lsfx "se_pause.wav") (lsfx "se_select00.wav")
		 (lsfx "se_timeout.wav") (lsfx "se_timeout2.wav"))))
(define (unload-sfx)
  ;; meh, todo on unloading all the individual sounds
  (set! sounds #f))

(define-record-type txbundle
  (fields
   reimu
   enemy1
   hud
   misc
   bullet1 bullet2 bullet3 bullet4 bullet5 bullet6))
(define (load-textures)
  (define (ltex file)
	(raylib:load-texture (string-append "assets/img/" file)))  
  (make-txbundle
   (ltex "reimu.png")
   (ltex "enemy1.png")
   (ltex "ui_bg.png")
   (ltex "misc.png")
   (ltex "bullet1.png")
   (ltex "bullet2.png")
   (ltex "bullet3.png")
   (ltex "bullet4.png")
   (ltex "bullet5.png")
   (ltex "bullet6.png")))
(define (unload-textures textures)
  (raylib:unload-texture (txbundle-reimu textures))
  (raylib:unload-texture (txbundle-hud textures))
  (raylib:unload-texture (txbundle-bullet1 textures))
  (raylib:unload-texture (txbundle-bullet2 textures))
  (raylib:unload-texture (txbundle-bullet3 textures))
  (raylib:unload-texture (txbundle-bullet4 textures))
  (raylib:unload-texture (txbundle-bullet5 textures))
  (raylib:unload-texture (txbundle-bullet6 textures))
  (raylib:unload-texture (txbundle-enemy1 textures))
  (raylib:unload-texture (txbundle-misc textures)))

(define-record-type sprite-descriptor
  (fields
   tx-accessor
   bounds
   center-shift))

(define (make-sprite-data)
  (let ([ret (make-eq-hashtable)]
		[shift8 (vec2 -8.0 -8.0)]
		[shift16 (vec2 -16.0 -16.0)])
	(define (make type accessor x y width height shift)
	  (hashtable-set!
	   ret type
	   (make-sprite-descriptor accessor
							   (make-rectangle
								(inexact x) (inexact y)
								(inexact width) (inexact height))
							   shift)))
	;; bullets
	(make 'pellet-red txbundle-bullet2 176 0 16 16 shift8)
	(make 'pellet-magenta txbundle-bullet2 176 32 16 16 shift8)
	(make 'pellet-blue txbundle-bullet2 176 80 16 16 shift8)
	(make 'pellet-cyan txbundle-bullet2 176 96 16 16 shift8)
	(make 'pellet-green txbundle-bullet2 176 112 16 16 shift8)
	(make 'pellet-yellow txbundle-bullet2 176 160 16 16 shift8)
	(make 'pellet-orange txbundle-bullet2 176 192 16 16 shift8)
	(make 'pellet-gray txbundle-bullet2 176 208 16 16 shift8)
	(make 'pellet-white txbundle-bullet2 176 224 16 16 shift8)
	(make 'small-star-red txbundle-bullet2 96 0 16 16 shift8)
	(make 'small-star-magenta txbundle-bullet2 96 32 16 16 shift8)
	(make 'small-star-blue txbundle-bullet2 96 64 16 16 shift8)
	(make 'small-star-cyan txbundle-bullet2 96 96 16 16 shift8)
	(make 'small-star-green txbundle-bullet2 96 128 16 16 shift8)
	(make 'small-star-yellow txbundle-bullet2 96 160 16 16 shift8)
	(make 'small-star-orange txbundle-bullet2 96 192 16 16 shift8)
	(make 'small-star-white txbundle-bullet2 96 224 16 16 shift8)
	(make 'small-star-black txbundle-bullet2 96 240 16 16 shift8)
	(make 'big-star-red txbundle-bullet2 224 0 32 32 shift16)
	(make 'big-star-magenta txbundle-bullet2 224 32 32 32 shift16)
	(make 'big-star-blue txbundle-bullet2 224 64 32 32 shift16)
	(make 'big-star-cyan txbundle-bullet2 224 96 32 32 shift16)
	(make 'big-star-green txbundle-bullet2 224 128 32 32 shift16)
	(make 'big-star-yellow txbundle-bullet2 224 160 32 32 shift16)
	(make 'big-star-orange txbundle-bullet2 224 192 32 32 shift16)
	(make 'big-star-white txbundle-bullet2 224 224 32 32 shift16)

	;; enemies
	(make 'red-fairy txbundle-enemy1 0 384 32 32 shift16)

	;; misc
	(make 'focus-sigil txbundle-misc 128 0 64 64 (vec2 -32.0 -32.0))
	(make 'mainshot txbundle-reimu 192 160 64 16 (vec2 -54.0 -8.0))
	ret))

(define sprite-data (make-sprite-data))

(define (draw-sprite textures sprite-id x y color)
  (let ([data (hashtable-ref sprite-data sprite-id #f)])
	(raylib:draw-texture-rec
	 ((sprite-descriptor-tx-accessor data) textures)
	 (sprite-descriptor-bounds data)
	 (v2+ (vec2 x y) (sprite-descriptor-center-shift data))
	 color)))

(define (draw-sprite-with-rotation textures sprite-id rotation x y color)
  (let* ([data (hashtable-ref sprite-data sprite-id #f)]
		 [center-shift (sprite-descriptor-center-shift data)])
	(raylib:push-matrix)
	(raylib:translatef x y 0.0)
	(raylib:rotatef rotation 0.0 0.0 1.0)
	(raylib:translatef (+ (v2x center-shift)) (+ (v2y center-shift)) 0.0)
	(raylib:draw-texture-rec
	 ((sprite-descriptor-tx-accessor data) textures)
	 (sprite-descriptor-bounds data)
	 v2zero
	 color)
	(raylib:pop-matrix)))

(define (constantly x)
  (lambda _args x))

(define (vector-for-each-truthy f v)
  (vector-for-each
   (lambda (e) (when e (f e)))
   v))

(define (vector-for-each-indexed f v)
  (define len (vector-length v))
  (let loop ([i 0])
	(when (< i len)
	  (f i (vector-ref v i)))))

(define (packcolor r g b a)
  (bitwise-ior (bitwise-arithmetic-shift-left r 24)
			   (bitwise-arithmetic-shift-left g 16)
			   (bitwise-arithmetic-shift-left b 8)
			   a))
(define red (packcolor 230 41 55 255))

;; [31-416] x bounds of playfield in the hud texture
;; [15-463] y bounds of playfield in the hud texture
;; idea is to have logical game x in [-192, 192] (192 is (416-31)/2, roughly), and y in [0, 448] 
;; logical game 0, 0 is at gl (31+(416-31)/2), 15
;; offset logical coords by 223, 15 to get to GL coords for render render
(define +playfield-render-offset-x+ 223)
(define +playfield-render-offset-y+ 15)
(define +playfield-min-x+ -192)
(define +playfield-min-y+ 0)
(define +playfield-max-x+ 192)
(define +playfield-max-y+ 448)
(define +oob-bullet-despawn-fuzz+ 10)

(define frames 0) ;; Number of frames the current stage has been running
(define iframes 0) ;; Remaining frames of invincibility
(define show-hitboxes #f)
(define graze 0)
(define paused #f)
(define current-boss-name #f)
(define current-spell-name #f)
(define current-boss-timer-frames 0.0) ;; will be converted to seconds for display
(define graze-radius 22.0)
(define hit-radius 3.0)
(define player-x 0.0)
(define player-y (- +playfield-max-y+ 10.0))
(define player-speed 200)
(define player-xv 0.0)
(define player-yv 0.0)

(define (player-invincible?)
  (positive? iframes))

(define (clamp v lower upper)
  (max (min v upper) lower))

(define ojamajo-carnival #f)
(define (load-audio)
  (raylib:init-audio-device)
  (set! ojamajo-carnival (raylib:load-music-stream "assets/bgm/ojamajo_carnival.wav")))
(define (unload-audio)
  (raylib:unload-music-stream ojamajo-carnival))

(define-record-type bullet
  (fields
   type
   color
   (mutable x)
   (mutable y)
   (mutable facing) ;; radians
   (mutable speed)
   (mutable grazed)
   (mutable lifespan)
   ;; extra hashtable for scratch pad. not set by default, allocate manually if needed
   (mutable extras)))

(define live-bullets (make-vector 4096 #f))

(define (vector-index elem v)
  (let ([len (vector-length v)])
	(let loop ([i 0])
	  (if (>= i len)
		  #f
		  (let ([elemi (vector-ref v i)])
			(if (eq? elem elemi)
				i
				(loop (add1 i))))))))

(define (vector-popcnt v)
  (let ([len (vector-length v)])
	(let loop ([i 0]
			   [n 0])
	  (if (>= i len)
		  n
		  (let ([elemi (vector-ref v i)])
			(if elemi
				(loop (add1 i) (add1 n))
				(loop (add1 i) n)))))))

(define (spawn-bullet type x y facing speed control-function)
  (let ([idx (vector-index #f live-bullets)])
	(unless idx
	  (error 'spawn-bullet "No more open bullet slots"))
	(let ([blt (make-bullet type 'white x y facing speed #f 0 #f)])
	  (vector-set! live-bullets idx blt)
	  (spawn-task "bullet"
				  (lambda () (control-function blt))
				  (lambda () (eq? blt (vector-ref live-bullets idx)))))))

(define (delete-bullet bullet)
  (let ([idx (vector-index bullet live-bullets)])
	(when idx
	  (vector-set! live-bullets idx #f))))

(define (despawn-out-of-bound-bullet bullet)
  (let ([x (bullet-x bullet)]
		[y (bullet-y bullet)])
	(when (or (> x (+ +playfield-max-x+ +oob-bullet-despawn-fuzz+))
			  (< x (- +playfield-min-x+ +oob-bullet-despawn-fuzz+))
			  (< y (- +playfield-min-y+ +oob-bullet-despawn-fuzz+))
			  (> y (+ +playfield-max-y+ +oob-bullet-despawn-fuzz+)))
	  (delete-bullet bullet))))

(define (bullet-family type)
  (case type
	((pellet-white pellet-gray pellet-orange pellet-yellow
	  pellet-green pellet-cyan pellet-blue pellet-magenta pellet-red)
	 'pellet)
	((small-star-red small-star-magenta small-star-blue small-star-cyan
	  small-star-green small-star-yellow small-star-orange
	  small-star-white small-star-black)
	 'small-star)
	((big-star-red big-star-magenta big-star-blue big-star-cyan
	  big-star-green big-star-yellow big-star-orange big-star-white)
	 'big-star)))

(define (bullet-hit-radius type)
  (case (bullet-family type)
	((pellet) 2.0)
	((small-star) 3.7)
	((big-star) 5.5)
	(else 0.0)))

(define (draw-bullets textures)
  (define (each bullet)
	(let ([render-x (+ (bullet-x bullet) +playfield-render-offset-x+)]
		  [render-y (+ (bullet-y bullet) +playfield-render-offset-y+)]
		  [type (bullet-type bullet)])
	  (case (bullet-family type)
		((pellet)
		 (draw-sprite textures type render-x render-y #xffffffff))
		((small-star big-star)
		 (draw-sprite-with-rotation textures type (mod (* frames 5.0) 360.0)
									render-x render-y #xffffffff)))
	  (when show-hitboxes
		(raylib:draw-circle-v render-x render-y (bullet-hit-radius type)
							  red))))
  (vector-for-each-truthy each live-bullets))

(define-record-type enm
  (fields
   type
   (mutable x)
   (mutable y)
   (mutable health)
   ;; extra hashtable for scratch pad. not set by default, allocate manually if needed
   (mutable extras)))
(define live-enm (make-vector 256 #f))

(define (spawn-enemy type x y health control-function)
  (let ((idx (vector-index #f live-enm)))
	(unless idx
	  (error 'spawn-enemy "No more open enemy slots!"))
	(let ([enemy (make-enm type x y health #f)]) ;; todo allow extras to be passed in
	  (vector-set! live-enm idx enemy)
	  (spawn-task
	   (symbol->string type)
	   (lambda () (control-function enemy))
	   (lambda () (eq? enemy (vector-ref live-enm idx)))))))

(define (delete-enemy enm)
  (let ([idx (vector-index enm live-enm)])
	(when idx
	  (vector-set! live-enm idx #f))))

(define (prune-dead-enemies)
  (let loop ([idx 0])
	(when (< idx (vector-length live-enm))
	  (let ([enemy (vector-ref live-enm idx)])
		(when (and enemy (<= (enm-health enemy) 0))
		  (raylib:play-sound (sebundle-enmdie sounds))
		  (vector-set! live-enm idx #f)))
	  (loop (add1 idx)))))

(define (process-collisions)
  (define (each bullet)
	(when (and (not (bullet-grazed bullet))
			   (check-collision-circles
				player-x player-y graze-radius
				(bullet-x bullet) (bullet-y bullet)
				(bullet-hit-radius (bullet-type bullet))))
	  (set! graze (add1 graze))
	  (bullet-grazed-set! bullet #t)
	  (raylib:play-sound (sebundle-graze sounds)))

	(when (and (not (player-invincible?))
			   (check-collision-circles
				player-x player-y hit-radius
				(bullet-x bullet) (bullet-y bullet)
				(bullet-hit-radius (bullet-type bullet))))
	  (raylib:play-sound (sebundle-playerdie sounds))
	  (set! iframes 180)))
  (vector-for-each-truthy each live-bullets))


(define (enm-hurtbox enm)
  (case (and enm (enm-type enm))
	((red-fairy)
	 (values (- (enm-x enm) 16)
			 (- (enm-y enm) 16)
			 32 32))))

(define (draw-enemies textures)
  (define (each enm)
	(let ((render-x (+ (enm-x enm) +playfield-render-offset-x+))
		  (render-y (+ (enm-y enm) +playfield-render-offset-y+)))
	  (case (enm-type enm)
		((red-fairy)
		 (draw-sprite textures 'red-fairy render-x render-y -1))))
	(when show-hitboxes
	  (let-values ([(x y w h) (enm-hurtbox enm)])
		(raylib:draw-rectangle-rec
		 (+ x +playfield-render-offset-x+)
		 (+ y +playfield-render-offset-y+) w h red))))
  (vector-for-each-truthy each live-enm))

;; Remaining todo:
;; get practice writing some basic patterns:
;; - perfect freeze?
;; - TD tojiko midspell zigzags
;; - outward-multiplying ring (each bullet splits into 3, then each of those splits, into 3, etc.)

(define (perfect-freeze-bullet blt)
  (do ([i 0 (add1 i)])
	  ((= i 240))
	(linear-step blt)
	(yield))
  (bullet-speed-set! blt 0)
  (wait 300)
  (let loop ()
	(linear-step blt)
	(bullet-speed-set! blt (+ (bullet-speed blt) 5))
	(yield)
	(loop)))

(define (linear-step-forever blt)
  (let loop ()
	(linear-step blt)
	(yield)
	(loop)))

(define (linear-step blt)
  (let ([facing (bullet-facing blt)]
		[speed (bullet-speed blt)])
	(bullet-x-set! blt (+ (bullet-x blt) (* speed (cos facing))))
	(bullet-y-set! blt (+ (bullet-y blt) (* speed (sin facing))))))

(define-record-type miscent
  (fields type (mutable x) (mutable y)))

(define live-misc-ents
  (make-vector 512 #f))

(define (spawn-misc-ent ent)
  (let ((idx (vector-index #f live-misc-ents)))
	(unless idx
	  (error 'spawn-misc-ent "No more open misc entity slots!"))
	(vector-set! live-misc-ents idx ent)))

(define (delete-misc-ent ent)
  (let ((idx (vector-index ent live-misc-ents)))
	(when idx
	  ;; tolerate killing already-removed/dead
	  (vector-set! live-misc-ents idx #f))))

(define (tick-misc-ents)
  (define (each ent)
	(case (miscent-type ent)
	  ((mainshot)
	   (miscent-y-set! ent (- (miscent-y ent) 10.0))
	   (call/1cc
		(lambda (return)
		  (let ((hitbox (make-rectangle
						 (- (miscent-x ent) 6) (- (miscent-y ent) 10)
						 12 16)))
			(vector-for-each-truthy
			 (lambda (enm)
			   (let-values ([(ehx ehy ehw ehh) (enm-hurtbox enm)])
				 (when (check-collision-recs
						ehx ehy ehw ehh
						(- (miscent-x ent) 6)
						(- (miscent-y ent) 6)
						12 16)
				   (delete-misc-ent ent)
				   (enm-health-set! enm (- (enm-health enm) 50))
				   (return))))
			 live-enm))
		  (when (< (miscent-y ent) +playfield-min-y+)
			(delete-misc-ent ent)))))))
  (vector-for-each-truthy each live-misc-ents))

(define (draw-misc-ents textures)
  (define (each ent)
	(case (miscent-type ent)
	  ((mainshot)
	   (let ((render-x (+ +playfield-render-offset-x+ (miscent-x ent)))
			 (render-y (+ +playfield-render-offset-y+ (miscent-y ent))))
		 (draw-sprite-with-rotation
		  textures 'mainshot -90.0
		  render-x render-y -1)
		 (when show-hitboxes
		   (raylib:draw-rectangle-rec
			(- render-x 6) (- render-y 10) 12 16
			red))))))
  (vector-for-each-truthy each live-misc-ents))

(define (ease-cubic-to x y duration enm)
  (define x0 (enm-x enm))
  (define y0 (enm-y enm))
  (do ([i 0 (add1 i)])
	  ((> i duration))
	(let* ([progress (/ i duration)]
		   [eased (ease-out-cubic (clamp progress 0.0 1.0))]
		   [x (+ x0 (* eased (- x x0)))]
		   [y (+ y0 (* eased (- y y0)))])
	  (enm-x-set! enm x)
	  (enm-y-set! enm y))
	(yield)))

(define (test-fairy-control enm)
  (define (pick-next-position)
	(let* ((x (enm-x enm))
		   (y (enm-y enm))
		   (angle (random (* 2 pi))) ;; XXX(replays): rng access
		   (magnitude 50.0)
		   (dx (* magnitude (cos angle)))
		   (dy (* magnitude (sin angle)))
		   (nx (clamp (+ x dx) +playfield-min-x+ +playfield-max-x+))
		   (ny (clamp (+ y dy) +playfield-min-y+ +playfield-max-y+)))
	  (values nx ny)))
  (let loop ()
	(define-values (x y) (pick-next-position))
	(ease-cubic-to x y 90 enm)
	(loop))
  )

(define (handle-input)
  ;; level triggered stuff
  (unless paused
	(when (raylib:is-key-down key-left)
	  (set! player-xv (sub1 player-xv)))
	(when (raylib:is-key-down key-right)
	  (set! player-xv (add1 player-xv)))
	(when (raylib:is-key-down key-up)
	  (set! player-yv (sub1 player-yv)))
	(when (raylib:is-key-down key-down)
	  (set! player-yv (add1 player-yv)))
	(when (and (raylib:is-key-down key-z)
			   (zero? (mod frames 5))) ;; todo separate counter
	  (let ((y (- player-y 20)))
		(spawn-misc-ent (make-miscent 'mainshot (- player-x 10) y))
		(spawn-misc-ent (make-miscent 'mainshot (+ player-x 10) y))
		(raylib:play-sound (sebundle-playershoot sounds)))))
  ;; edge triggered stuff
  (let loop ([k (raylib:get-key-pressed)])
	(cond
	 [(= k key-f3)
	  (set! show-hitboxes (not show-hitboxes))]
	 [(= k key-escape)
	  (set! paused (not paused))
	  (if paused
		  (let ()
			(raylib:play-sound (sebundle-pause sounds))
			(raylib:pause-music-stream ojamajo-carnival))
		  (raylib:resume-music-stream ojamajo-carnival))]
	 [(= k key-space)
	  (spawn-enemy 'red-fairy 0.0 100.0 200.0 test-fairy-control)
	  ;; (spawn-task
	  ;;  "spawner"
	  ;;  (lambda ()
	  ;; 	 (do ((i 0 (add1 i)))
	  ;; 		 ((= i 300))
	  ;; 	   (let ([ang (- (random (* 2 pi)) pi)])
	  ;; 		 (spawn-bullet 'big-star-red 0.0 100.0 ang 2 linear-step-forever))
	  ;; 	   (yield)))
	  ;;  (constantly #t))
	  ])
	(unless (zero? k)
	  (loop (raylib:get-key-pressed)))))

(define (handle-player-movement)
  (when (not (and (zero? player-xv) (zero? player-yv)))
    (let* ((delta-time (raylib:get-frame-time))
           (velocity (* player-speed delta-time (if (raylib:is-key-down key-left-shift) 0.5 1)))
           (normalized-direction (v2unit (vec2 player-xv player-yv)))
           (acceleration (v2* normalized-direction velocity))
           (new-x (+ player-x (v2x acceleration)))
           (new-y (+ player-y (v2y acceleration))))
	  ;; todo: refine this so that you can bottomdrag instead of not moving at all
      (when (not (or (> new-x +playfield-max-x+) (> new-y +playfield-max-y+) (< new-y +playfield-min-y+) (< new-x +playfield-min-x+)))
        (set! player-x new-x)
        (set! player-y new-y)))
    (set! player-xv 0.0)
    (set! player-yv 0.0)))

(define focus-sigil-strength 0.0)
(define (draw-player textures)
  (let* ([render-player-x (+ player-x +playfield-render-offset-x+)]
		 [render-player-y (+ player-y +playfield-render-offset-y+)])
	;; player sprite (todo: directional moving sprites)
	(let ((x-texture-index (truncate (mod (/ frames 9) 8)))
		  (iframe-blink
		   (if (and (player-invincible?)
					(< (mod frames 4) 2))
			   (packcolor 64 64 255 255)
			   #xffffffff)))
	  (raylib:draw-texture-rec
	   (txbundle-reimu textures)
	   (make-rectangle (* 32.0 x-texture-index) 0.0 32.0 48.0)
	   (vec2 (- render-player-x 16.0) (- render-player-y 24.0))
	   iframe-blink))
	
	;; focus sigil
	(if (raylib:is-key-down key-left-shift)
		(when (< focus-sigil-strength 1.0)
		  (set! focus-sigil-strength (min (+ focus-sigil-strength 0.1) 1.0)))
		(when (> focus-sigil-strength 0.0)
		  (set! focus-sigil-strength (max (- focus-sigil-strength 0.1) 0.0))))
	(when (> focus-sigil-strength 0.0)
	  (raylib:push-matrix)
	  (raylib:translatef render-player-x render-player-y 0.0) ;; move to where we are
	  (raylib:rotatef (mod frames 360.0) 0.0 0.0 1.0) ;; spin
	  (draw-sprite textures 'focus-sigil
				   0.0 0.0 ;; manually translated to final position above
				   (packcolor 255 255 255 (exact (round (* 255 focus-sigil-strength)))))
	  (raylib:pop-matrix))

	(when show-hitboxes
	  (raylib:draw-circle-v render-player-x render-player-y graze-radius
							(packcolor 0 228 48 255))
	  (raylib:draw-circle-v render-player-x render-player-y hit-radius
						    red))))

(define (render-all textures)
  (raylib:clear-background 0)
  (draw-player textures)
  (draw-enemies textures)
  (draw-misc-ents textures)
  (draw-bullets textures)

  (when paused
	(raylib:draw-text "PAUSED" 175 150 28 (packcolor 200 122 255 255)))

  (raylib:draw-texture (txbundle-hud textures) 0 0 #xffffffff)
  (raylib:draw-text (format "MISC: ~d" (vector-popcnt live-misc-ents))
					500 350
					18 -1)
  (raylib:draw-text (format "GRAZE: ~d" graze)
					500 375
					18 -1)
  (raylib:draw-text (format "ENM: ~d" (vector-popcnt live-enm))
					500 400
					18 -1)
  (raylib:draw-text (format "BLT: ~d" (vector-popcnt live-bullets))
					500 425
					18 -1)
  (raylib:draw-fps 500 450))

(define (main)
  (collect-notify #t)
  (raylib:init-window 640 480 "thdawn")
  (raylib:set-target-fps 60)
  (raylib:set-exit-key 0)
  (load-audio)
  (load-sfx)
  (let ([textures (load-textures)])
	(raylib:play-music-stream ojamajo-carnival)
	(let loop ()
	  (unless (raylib:window-should-close)
		(handle-input)
		(raylib:update-music-stream ojamajo-carnival)
		(unless paused
		  (when (positive? iframes)
			(set! iframes (sub1 iframes)))
		  (handle-player-movement)
		  (vector-for-each-truthy
		   (lambda (blt) (despawn-out-of-bound-bullet blt))
		   live-bullets)
		  (prune-dead-enemies)
		  (run-tasks)
		  (tick-misc-ents)
		  (process-collisions))
		(raylib:begin-drawing)
		(render-all textures)
		(raylib:end-drawing)
		(unless paused
		  (set! frames (add1 frames)))
		(loop)))
	(unload-audio)
	(unload-textures textures)
	(unload-sfx)
	(raylib:close-window)))
