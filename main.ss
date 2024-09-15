#!chezscheme
;; this is needed separately to bring add-prefix into scope when treating this
;; file as a r6rs Program
(import (chezscheme))
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
(define key-y 89)
(define key-z 90)
(define pi 3.141592)

(define (ease-out-cubic x)
  (- 1 (expt (- 1 x) 3.0)))
(define (lerp a b progress)
  (+ a (* progress (- b a))))

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
   timeout timeoutwarn item))
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
		 (lsfx "se_timeout.wav") (lsfx "se_timeout2.wav")
		 (lsfx "se_item00.wav"))))
(define (unload-sfx)
  (define rtd (record-type-descriptor sebundle))
  (define num-sounds (vector-length (record-type-field-names rtd)))
  (for-each (lambda (i)
			  (raylib:unload-sound ((record-accessor rtd i) sounds)))
			(iota num-sounds))
  (set! sounds #f))

(define-record-type txbundle
  (fields
   reimu
   enemy1
   hud
   misc item
   bullet1 bullet2 bullet3 bullet4 bullet5 bullet6
   bg1 bg2 bg3 bg4
   bulletcancel hint
   ))
(define (load-textures)
  (define (ltex file)
	(raylib:load-texture (string-append "assets/img/" file)))  
  (make-txbundle
   (ltex "reimu.png")
   (ltex "enemy1.png")
   (ltex "ui_bg.png")
   (ltex "misc.png") (ltex "item.png")
   (ltex "bullet1.png") (ltex "bullet2.png") (ltex "bullet3.png")
   (ltex "bullet4.png") (ltex "bullet5.png") (ltex "bullet6.png")
   (ltex "background_1.png") (ltex "background_2.png")
   (ltex "background_3.png") (ltex "background_4.png")
   (ltex "etbreak.png") (ltex "hint.png")))
(define (unload-textures textures)
  (define rtd (record-type-descriptor txbundle))
  (define num-textures (vector-length (record-type-field-names rtd)))
  (for-each (lambda (i)
			  (raylib:unload-texture ((record-accessor rtd i) textures)))
			(iota num-textures)))

(define-record-type fontbundle
  (fields
   bubblegum
   cabin))
(define (load-fonts)
  (make-fontbundle
   (raylib:load-font "assets/font/BubblegumSans-Regular.ttf")
   (raylib:load-font "assets/font/Cabin-Regular.ttf")))
(define (unload-fonts fonts)
  (define rtd (record-type-descriptor fontbundle))
  (define num-fonts (vector-length (record-type-field-names rtd)))
  (for-each (lambda (i)
			  (raylib:unload-font ((record-accessor rtd i) fonts)))
			(iota num-fonts)))

(define-record-type sprite-descriptor
  (fields
   tx-accessor
   bounds
   center-shift))

(define (make-sprite-data)
  (define ret (make-hashtable symbol-hash eq?))
  (define shift8 (vec2 -8.0 -8.0))
  (define shift16 (vec2 -16.0 -16.0))
  (define (make type accessor x y width height shift)
	(symbol-hashtable-set!
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

  ;; items
  (make 'life-frag txbundle-item 0 32 32 32 shift16)
  (make 'big-piv txbundle-item 0 64 32 32 shift16)
  (make 'life txbundle-item 0 96 32 32 shift16)
  (make 'bomb-frag txbundle-item 0 128 32 32 shift16)

  (make 'point txbundle-item 32 0 32 32 shift16)
  (make 'small-piv txbundle-item 32 96 32 32 shift16)
  (make 'bomb txbundle-item 32 128 32 32 shift16)

  ;; misc
  (make 'focus-sigil txbundle-misc 128 0 64 64 (vec2 -32.0 -32.0))
  (make 'mainshot txbundle-reimu 192 160 64 16 (vec2 -54.0 -8.0))
  (make 'life-third txbundle-hint 288 16 16 16 v2zero)
  (make 'life-two-thirds txbundle-hint 320 16 16 16 v2zero)
  (make 'life-full txbundle-hint 304 0 16 16 v2zero)
  (make 'bomb-third txbundle-hint 288 32 16 16 v2zero)
  (make 'bomb-two-thirds txbundle-hint 320 32 16 16 v2zero)
  (make 'bomb-full txbundle-hint 336 0 16 16 v2zero)
  ret)

(define sprite-data (make-sprite-data))

(define (draw-sprite textures sprite-id x y color)
  (define data (symbol-hashtable-ref sprite-data sprite-id #f))
  (raylib:draw-texture-rec
   ((sprite-descriptor-tx-accessor data) textures)
   (sprite-descriptor-bounds data)
   (v2+ (vec2 x y) (sprite-descriptor-center-shift data))
   color))

(define (draw-sprite-with-rotation textures sprite-id rotation x y color)
  (define data (symbol-hashtable-ref sprite-data sprite-id #f))
  (define center-shift (sprite-descriptor-center-shift data))
  (raylib:push-matrix)
  (raylib:translatef x y 0.0)
  (raylib:rotatef (inexact rotation) 0.0 0.0 1.0)
  (raylib:translatef (+ (v2x center-shift)) (+ (v2y center-shift)) 0.0)
  (raylib:draw-texture-rec
   ((sprite-descriptor-tx-accessor data) textures)
   (sprite-descriptor-bounds data)
   v2zero
   color)
  (raylib:pop-matrix))

(define constantly
  ;; some common stuff to avoid gc spamming
  (let* ([constantly-t (lambda _args #t)])
	(lambda (x)
	  (cond
	   [(eq? #t x) constantly-t]
	   [else (lambda _args x)]))))

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
;; offset logical coords by 223, 15 to get to GL coords for render
(define +playfield-render-offset-x+ 223)
(define +playfield-render-offset-y+ 15)
(define +playfield-min-x+ -192)
(define +playfield-min-y+ 0)
(define +playfield-max-x+ 192)
(define +playfield-max-y+ 448)
(define +poc-y+ 120)
(define +oob-bullet-despawn-fuzz+ 10)

;; Number of frames the current stage has been running. Does not increment when paused
(define frames 0)
;; Always increments by one per frame no matter what. Should not be used often.
(define true-frames 0)
(define iframes 0) ;; Remaining frames of invincibility
(define respawning 0) ;; Nonzero if going through the respawn animation
(define +respawning-max+ 60)
(define show-hitboxes #f)
(define graze 0)
(define paused #f)
(define current-boss-name #f)
(define current-spell-name #f)
(define current-boss-timer-frames 0.0) ;; will be converted to seconds for display
(define graze-radius 22.0)
(define hit-radius 3.0)
(define player-x 0.0)
(define +initial-player-y+ (- +playfield-max-y+ 20.0))
(define player-y +initial-player-y+)
(define item-value 10000)
(define current-score 0)
;; fragments are stored as Scheme's fractional data types
(define life-stock 2)
(define bomb-stock 3)

(define (player-invincible?)
  (positive? iframes))

(define (clamp v lower upper)
  (max (min v upper) lower))

(define ojamajo-carnival #f)
(define (load-audio)
  (raylib:init-audio-device)
  (set! ojamajo-carnival (raylib:load-music-stream "assets/bgm/ojamajo_carnival.wav")))
(define (unload-audio)
  (raylib:unload-music-stream ojamajo-carnival)
  (set! ojamajo-carnival #f)
  (raylib:close-audio-device))

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
  (define len (vector-length v))
  (let loop ([i 0])
	(if (fx>= i len)
		#f
		(let ([elemi (vector-ref v i)])
		  (if (eq? elem elemi)
			  i
			  (loop (add1 i)))))))

(define (vector-popcnt v)
  (define len (vector-length v)) 
  (let loop ([i 0]
			 [n 0])
	(if (fx>= i len)
		n
		(let ([elemi (vector-ref v i)])
		  (if elemi
			  (loop (add1 i) (add1 n))
			  (loop (add1 i) n))))))

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

(define (cancel-bullet bullet)
  (spawn-particle (make-particle
				   'cancel
				   (bullet-x bullet) (bullet-y bullet)
				   23 0))
  (delete-bullet bullet))

(define (cancel-bullet-with-drop bullet drop)
  (define ent (make-miscent drop (bullet-x bullet) (bullet-y bullet) -2.0 0.1 0 #f))
  (spawn-misc-ent ent)
  (spawn-task "delayed autocollect"
			  (lambda () (wait 30) (miscent-autocollect-set! ent #t))
			  (constantly #t))
  (cancel-bullet bullet))

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
		 (draw-sprite-with-rotation textures type (mod (* frames 5) 360)
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
   ;; alist of (miscent type . count) to drop on death.
   drops
   ;; extra hashtable for scratch pad. not set by default, allocate manually if needed
   (mutable extras)))
(define live-enm (make-vector 256 #f))
(define default-drop '((point . 1)))

(define (spawn-enemy type x y health control-function drops)
  (let ((idx (vector-index #f live-enm)))
	(unless idx
	  (error 'spawn-enemy "No more open enemy slots!"))
	(let ([enemy (make-enm type x y health drops #f)]) ;; todo allow extras to be passed in
	  (vector-set! live-enm idx enemy)
	  (spawn-task
	   (symbol->string type)
	   (lambda () (control-function enemy))
	   (lambda () (eq? enemy (vector-ref live-enm idx)))))))

(define (delete-enemy enm)
  (let ([idx (vector-index enm live-enm)])
	(when idx
	  (vector-set! live-enm idx #f))))

(define (spawn-drops enm)
  (define drops (enm-drops enm))
  (define x (enm-x enm))
  (define y (enm-y enm))
  (for-each
   (lambda (drop)
	 (define type (car drop))
	 (define count (cdr drop))
	 (do [(i 0 (fx1+ i))]
		 [(fx>= i count)]
	   ;; skip fuzz on the very first item, so that in the simple case of a single
	   ;; drop, it doesn't awkwardly get spawned away from the enemy
	   (let* ([skip-fuzz (and (fxzero? i)
							  (eq? drop (car drops)))]
			  ;; xxx(replays) random access
			  [fuzz-x (if skip-fuzz 0.0 (- (random 18.0) 9.0))]
			  [fuzz-y (if skip-fuzz 0.0 (- (random 8.0) 4.0))])
		 (spawn-misc-ent
		  (make-miscent
		   type
		   (+ x fuzz-x) (+ y fuzz-y)
		   -3.0 0.1 0 #f)))))
   drops))

(define (prune-dead-enemies)
  (let loop ([idx 0])
	(when (< idx (vector-length live-enm))
	  (let ([enemy (vector-ref live-enm idx)])
		(when (and enemy (<= (enm-health enemy) 0))
		  (spawn-drops enemy)
		  (raylib:play-sound (sebundle-enmdie sounds))
		  (vector-set! live-enm idx #f)))
	  (loop (add1 idx)))))

(define (kill-player)
  (when (> life-stock 1)
	(set! life-stock (sub1 life-stock))) ;; todo gameovering
  (raylib:play-sound (sebundle-playerdie sounds))
  (set! iframes 180)
  (set! respawning +respawning-max+)
  (set! player-x 0.0)
  (set! player-y +initial-player-y+))

(define (process-collisions)
  (define (each bullet)
	(when (and (not (bullet-grazed bullet))
			   (check-collision-circles
				player-x player-y graze-radius
				(bullet-x bullet) (bullet-y bullet)
				(bullet-hit-radius (bullet-type bullet))))
	  (set! graze (fx1+ graze))
	  (when (fxzero? (mod graze 10))
		(set! item-value (fx+ 10 item-value)))
	  (bullet-grazed-set! bullet #t)
	  (raylib:play-sound (sebundle-graze sounds)))

	(when (check-collision-circles
		   player-x player-y hit-radius
		   (bullet-x bullet) (bullet-y bullet)
		   (bullet-hit-radius (bullet-type bullet)))
	  (cancel-bullet bullet)
	  (when (not (player-invincible?))
		(kill-player))))
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

(define-record-type particle
  (fields
   type
   (mutable x)
   (mutable y)
   max-age
   (mutable age)))

(define live-particles (make-vector 4096 #f))

(define (spawn-particle p)
  (let ([idx (vector-index #f live-particles)])
	(unless idx
	  (error 'spawn-particle "No more open particle slots!"))
	(vector-set! live-particles idx p)))

(define (delete-particle p)
  (let ([idx (vector-index p live-particles)])
	(when idx
	  ;; tolerate killing already-removed/dead
	  (vector-set! live-particles idx #f))))

(define (tick-particles)
  (define (each p)
	(particle-age-set! p (fx1+ (particle-age p)))
	(when (fx> (particle-age p) (particle-max-age p))
	  (delete-particle p)))
  (vector-for-each-truthy each live-particles))

(define (draw-particles textures)
  (define (each p)
	(define render-x (+ (particle-x p) +playfield-render-offset-x+))
	(define render-y (+ (particle-y p) +playfield-render-offset-y+))
	(case (particle-type p)
	  ([cancel]
	   (let* ([age (floor (/ (particle-age p) 3))]
			  [v (if (fx< age 4) 0.0 64.0)]
			  [u (* 64 (fxmod age 4))])
		 (raylib:draw-texture-pro
		  (txbundle-bulletcancel textures)
		  (make-rectangle u v 64.0 64.0)
		  (make-rectangle (- render-x 24.0) (- render-y 24.0) 48.0 48.0)
		  v2zero 0.0 -1)))))
  (vector-for-each-truthy each live-particles))

(define-record-type miscent
  (fields
   type
   (mutable x)
   (mutable y)
   (mutable vy)
   (mutable ay)
   (mutable lifespan)
   (mutable autocollect)))

(define (miscent-supports-autocollect? ent)
  (not (eq? (miscent-type ent) 'mainshot)))

(define live-misc-ents
  (make-vector 4096 #f))

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
	(define type (miscent-type ent))
	(define (do-standard-movement)
	  (let ([vy (miscent-vy ent)]
			[ay (miscent-ay ent)])
		(when (not (zero? vy))
	  	  (miscent-y-set! ent (+ (miscent-y ent) vy)))
		(when (not (zero? ay))
		  (miscent-vy-set! ent (+ vy ay)))))
	(case type
	  ((mainshot)
	   (do-standard-movement)
	   (call/1cc
		(lambda (return)
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
		   live-enm)
		  (when (< (miscent-y ent) +playfield-min-y+)
			(delete-misc-ent ent)))))
	  ([point life-frag big-piv life bomb-frag small-piv bomb]
	   (cond
		[(miscent-autocollect ent)
		 ;; todo dedupe with below
		 ;; todo acceleration?
		 (let ([dir-to-player (v2unit (vec2 (- player-x (miscent-x ent))
											(- player-y (miscent-y ent))))])
		   (miscent-x-set! ent (+ (miscent-x ent) (* (v2x dir-to-player) 8)))
		   (miscent-y-set! ent (+ (miscent-y ent) (* (v2y dir-to-player) 8))))]
		[(and
			(raylib:is-key-down key-left-shift)
			(check-collision-circle-rec
			 player-x player-y graze-radius
			 (- (miscent-x ent) 8) (- (miscent-y ent) 8)
			 16 16))
		 (let ([dir-to-player (v2unit (vec2 (- player-x (miscent-x ent))
											(- player-y (miscent-y ent))))])
		   (miscent-x-set! ent (+ (miscent-x ent) (* (v2x dir-to-player) 3)))
		   (miscent-y-set! ent (+ (miscent-y ent) (* (v2y dir-to-player) 3))))]
		[else (do-standard-movement)])
	   (when (check-collision-circle-rec
			  player-x player-y hit-radius
			  (- (miscent-x ent) 8) (- (miscent-y ent) 8)
			  16 16)
		 ;; todo life and bomb limits
		 (case type
		   ([point]
			(raylib:play-sound (sebundle-item sounds))
			(set! current-score (+ current-score item-value)))
		   ([life-frag]
			(set! life-stock (+ 1/3 life-stock))
			(when (integer? life-stock)
			  (raylib:play-sound (sebundle-extend sounds))))
		   ([life]
			(set! life-stock (add1 life-stock))
			(raylib:play-sound (sebundle-extend sounds)))
		   ([bomb-frag]
			(set! bomb-stock (+ 1/3 bomb-stock))
			(when (integer? bomb-stock)
			  (raylib:play-sound (sebundle-spellcapture sounds))))
		   ([bomb]
			(set! bomb-stock (add1 bomb-stock))
			(raylib:play-sound (sebundle-spellcapture sounds)))
		   ([small-piv]
			(raylib:play-sound (sebundle-item sounds))
			(set! item-value (+ 50 item-value))
			(set! current-score (+ 100 current-score)))
		   ([big-piv]
			(raylib:play-sound (sebundle-item sounds))
			(set! item-value (+ 200 item-value))
			(set! current-score (+ 1000 current-score))))
		 (delete-misc-ent ent))
	   (when (> (miscent-y ent) (+ +playfield-max-y+ 20))
		 (delete-misc-ent ent))))
	(miscent-lifespan-set! ent (add1 (miscent-lifespan ent))))
  (vector-for-each-truthy each live-misc-ents))

(define (miscent-should-spin? ent)
  (memq (miscent-type ent) '(point life life-frag bomb bomb-frag)))

(define (draw-misc-ents textures)
  (define (each ent)
	(define type (miscent-type ent))
	(define render-x (+ +playfield-render-offset-x+ (miscent-x ent)))
	(define render-y (+ +playfield-render-offset-y+ (miscent-y ent)))
	(case type
	  ((mainshot)
	   (draw-sprite-with-rotation
		textures 'mainshot -90
		render-x render-y -1)
	   (when show-hitboxes
		 (raylib:draw-rectangle-rec
		  (- render-x 6) (- render-y 10) 12 16
		  red)))
	  ([point life-frag big-piv life bomb-frag small-piv bomb]
	   (let ([lifespan (miscent-lifespan ent)])
		 (if (and (fx<= lifespan 24) (miscent-should-spin? ent))
			 (draw-sprite-with-rotation
			  textures type (* 45.0 (floor (/ lifespan 3)))
			  render-x render-y -1)
			 (draw-sprite textures type render-x render-y -1)))
	   (when show-hitboxes
		 (raylib:draw-rectangle-rec
		  (- render-x 8) (- render-y 8) 16 16
		  red)))))
  (vector-for-each-truthy each live-misc-ents))

(define (ease-cubic-to x y duration enm)
  (define x0 (enm-x enm))
  (define y0 (enm-y enm))
  (do ([i 0 (fx1+ i)])
	  ((fx> i duration))
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
  (when (and (not paused) (zero? respawning))
	(handle-player-movement)
	(when (and (raylib:is-key-down key-z)
			   (fxzero? (mod frames 5))) ;; todo separate counter
	  (let ((y (- player-y 20)))
		(spawn-misc-ent (make-miscent 'mainshot (- player-x 10) y
									  -10 0 0 #f))
		(spawn-misc-ent (make-miscent 'mainshot (+ player-x 10) y
									  -10 0 0 #f))
		(raylib:play-sound (sebundle-playershoot sounds)))))
  ;; edge triggered stuff
  (let loop ([k (raylib:get-key-pressed)])
	(cond
	 [(fx= k key-f3)
	  (set! show-hitboxes (not show-hitboxes))]
	 [(fx= k key-escape)
	  (set! paused (not paused))
	  (if paused
		  (let ()
			(raylib:play-sound (sebundle-pause sounds))
			(raylib:pause-music-stream ojamajo-carnival))
		  (raylib:resume-music-stream ojamajo-carnival))]
	 [(fx= k key-space)
	  
	  (spawn-enemy 'red-fairy 0.0 100.0 200.0 test-fairy-control '((bomb-frag . 1)))
	  ;; (spawn-task
	  ;;  "spawner"
	  ;;  (lambda ()
	  ;; 	 (do ((i 0 (add1 i)))
	  ;; 		 ((= i 300))
	  ;; 	   (let ([ang (- (random (* 2 pi)) pi)])
	  ;; 		 (spawn-bullet 'big-star-red 0.0 100.0 ang 2 linear-step-forever))
	  ;; 	   (yield)))
	  ;;  (constantly #t))
	  ]
	 [(fx= k key-y)
	  (vector-for-each-truthy
	   (lambda (blt) (cancel-bullet-with-drop blt 'small-piv))
	   live-bullets)]
	 [(= k 71)
	  (enable-object-counts #t)
	  (collect (collect-maximum-generation))
	  (display (object-counts))
	  (newline)
	  (enable-object-counts #f)
	  ]
	 )
	(unless (zero? k)
	  (loop (raylib:get-key-pressed)))))

(define (handle-player-movement)
  (define left-pressed (raylib:is-key-down key-left))
  (define right-pressed (raylib:is-key-down key-right))
  (define up-pressed (raylib:is-key-down key-up))
  (define down-pressed (raylib:is-key-down key-down))
  (define dir (vec2 (fl+ (if left-pressed -1.0 0.0) (if right-pressed 1.0 0.0))
					(fl+ (if up-pressed -1.0 0.0) (if down-pressed 1.0 0.0))))
  (when (not (and (flzero? (v2x dir)) (flzero? (v2y dir))))
	(let* ([speed (if (raylib:is-key-down key-left-shift) 2.25 4.125)]
		   [velvec (v2* (v2unit dir) speed)]
		   [new-x (clamp (fl+ player-x (v2x velvec))
						 +playfield-min-x+ +playfield-max-x+)]
		   [new-y (clamp (fl+ player-y (v2y velvec))
						 +playfield-min-y+ +playfield-max-y+)])
	  (set! player-x new-x)
	  (set! player-y new-y)))
  (when (< player-y +poc-y+)
	(vector-for-each-truthy
	 (lambda (ent)
	   (when (miscent-supports-autocollect? ent)
		 (miscent-autocollect-set! ent #t)))
	 live-misc-ents)))

(define (get-player-render-pos)
  (if (fxpositive? respawning)
	  (let ([progress (/ (fx- +respawning-max+ respawning)
						 +respawning-max+)]
			[start-y (+ +playfield-max-y+ 5.0)]
			[end-y +initial-player-y+])
		(values (inexact +playfield-render-offset-x+)
				(+ (lerp start-y end-y progress) +playfield-render-offset-y+)))
	  (values (+ player-x +playfield-render-offset-x+)
			  (+ player-y +playfield-render-offset-y+))))

(define focus-sigil-strength 0.0)
(define (draw-player textures)
  (define-values (render-player-x render-player-y)
	(get-player-render-pos))
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
						  red)))

(define (draw-hud textures fonts)
  (raylib:draw-texture (txbundle-hud textures) 0 0 #xffffffff)
  ;; todo actually make this look good
  (raylib:draw-text-ex
   (fontbundle-bubblegum fonts)
   (format "Score: ~:d" current-score)
   440 15 24.0 0.0 -1)
  (raylib:draw-text-ex
   (fontbundle-bubblegum fonts)
   (format "Life")
   440 45
   24.0 0.0 #xFF69FCFF)
  (raylib:draw-text-ex
   (fontbundle-bubblegum fonts)
   (format "Bomb")
   440 75
   24.0 0.0 #x72E57AFF)
  (raylib:draw-text-ex
   (fontbundle-bubblegum fonts)
   (format "Graze: ~d" graze)
   440 105
   24.0 0.0 -1)
  (raylib:draw-text-ex
   (fontbundle-bubblegum fonts)
   (format "Value: ~:d" item-value)
   440 135
   24.0 0.0 #x49D0FFFF)

  (let* ([start-x 490.0]
		 [y 48.0]
		 [whole-lives (floor life-stock)]
		 ;; will always be 0, 1, or 2
		 [fractional-lives (* 3 (- life-stock whole-lives))])
	(do [(i 0 (1+ i))]
		[(>= i whole-lives)]
	  (draw-sprite textures 'life-full (+ start-x (* 16.0 i)) y -1))
	(cond
	 [(fx= fractional-lives 1)
	  (draw-sprite textures 'life-third
				   (+ start-x (* 16.0 whole-lives)) y -1)]
	 [(fx= fractional-lives 2)
	  (draw-sprite textures 'life-two-thirds
				   (+ start-x (* 16.0 whole-lives)) y -1)]))

  (let* ([start-x 490.0]
		 [y 77.0]
		 [whole-bombs (floor bomb-stock)]
		 ;; will always be 0, 1, or 2
		 [fractional-bombs (* 3 (- bomb-stock whole-bombs))])
	(do [(i 0 (1+ i))]
		[(>= i whole-bombs)]
	  (draw-sprite textures 'bomb-full (+ start-x (* 16.0 i)) y -1))
	(cond
	 [(fx= fractional-bombs 1)
	  (draw-sprite textures 'bomb-third
				   (+ start-x (* 16.0 whole-bombs)) y -1)]
	 [(fx= fractional-bombs 2)
	  (draw-sprite textures 'bomb-two-thirds
				   (+ start-x (* 16.0 whole-bombs)) y -1)]))

  ;; todo: for prod release, hide this behind f3
  (raylib:draw-text (format "X: ~,2f / Y: ~,2f" player-x player-y)
					440 275
					18 -1)
  (raylib:draw-text (format "MEM: ~,2f MiB"
							(/ (current-memory-bytes)
							   (* 1024.0 1024.0)))
					440 300
					18 -1)
  (raylib:draw-text (format "FRAME: ~d" frames)
					440 325
					18 -1)
  (raylib:draw-text (format "MISC: ~d" (vector-popcnt live-misc-ents))
					440 350
					18 -1)
  (raylib:draw-text (format "ENM: ~d" (vector-popcnt live-enm))
					440 375
					18 -1)
  (raylib:draw-text (format "BLT: ~d" (vector-popcnt live-bullets))
					440 400
					18 -1)
  (raylib:draw-fps 440 450))

(define bg1-scroll 0.0)
(define bg2-scroll 0.0)
(define bg3-scroll 0.0)
(define screen-full-bounds
  (make-rectangle 0.0 0.0 640.0 480.0))
(define (render-all textures fonts)
  (raylib:clear-background 0) ;;#x42024aff) ;; todo: some variability :D
  (unless paused
	(let ([bg1-vel
		   ;; 800-900 0.5 1.0 1.5
		   ;; 3250 3550 accel
		   ;; 3600-5200 1.8 2.0 2.5
		   ;; 5500-6300 decel
		   ;; 6400 0.5 1.0 1.5
		   ;; 9150 9450 accel
		   ;; 9500-11100 1.8 2.0 2.5
		   ;; 11100 0.4 0.9 1.4 (or slower)
		   ;; 11850 1.8 2.0 2.5 (with brief accel)
		   ;; 12800 decel to 0.5 1.0 1.5
		   (cond
		    [else 1.8])]
		  [bg2-vel
		   (cond
			[else 2.1])]
		  [bg3-vel
		   (cond [else 2.7])])
	  (set! bg1-scroll (fl- bg1-scroll bg1-vel))
	  (set! bg2-scroll (fl- bg2-scroll bg2-vel))
	  (set! bg3-scroll (fl- bg3-scroll bg3-vel))))
  (raylib:draw-texture-pro (txbundle-bg1 textures)
						   (make-rectangle 0.0 bg1-scroll 256.0 224.0)
						   screen-full-bounds
						   v2zero 0.0 #xc0c0c0ff)
  (raylib:draw-texture-pro (txbundle-bg2 textures)
						   (make-rectangle 0.0 bg2-scroll 256.0 224.0)
						   screen-full-bounds
						   v2zero 0.0 #xc0c0c0ff)
  (raylib:draw-texture-pro (txbundle-bg3 textures)
						   (make-rectangle 0.0 bg3-scroll 256.0 224.0)
						   screen-full-bounds
						   v2zero 0.0 #xc0c0c0ff)
  ;; (raylib:draw-texture-pro (txbundle-bg4 textures)
  ;; 						   (make-rectangle 0.0 (mod (* frames -0.5) 224.0) 256.0 224.0)
  ;; 						   screen-full-bounds
  ;; 						   v2zero 0.0 -1)
  (when show-hitboxes
	(raylib:draw-line (+ +playfield-render-offset-x+ +playfield-min-x+)
					  (+ +playfield-render-offset-y+ +poc-y+)
					  (+ +playfield-render-offset-x+ +playfield-max-x+)
					  (+ +playfield-render-offset-y+ +poc-y+)
					  -1))
  (draw-player textures)
  (draw-enemies textures)
  (draw-misc-ents textures)
  (draw-particles textures)
  (draw-bullets textures)

  (when (and paused (< (mod true-frames 60) 30))
	(raylib:draw-text-ex
	 (fontbundle-bubblegum fonts)
	 "~ Paused ~"
	 175 150 32.0 0.0 (packcolor 200 122 255 255)))
  (draw-hud textures fonts))

(define (main)
  (collect-notify #t)
  (raylib:init-window 640 480 "thdawn")
  (raylib:set-target-fps 60)
  (raylib:set-exit-key 0)
  (load-audio)
  (load-sfx)
  (let ([textures (load-textures)]
		[fonts (load-fonts)])
	(raylib:play-music-stream ojamajo-carnival)
	(set! iframes 180)
	(let loop ()
	  (unless (raylib:window-should-close)
		(handle-input)
		(raylib:update-music-stream ojamajo-carnival)
		(unless paused
		  (when (positive? iframes)
			(set! iframes (sub1 iframes)))
		  (when (positive? respawning)
			(set! respawning (sub1 respawning)))
		  (vector-for-each-truthy
		   (lambda (blt) (despawn-out-of-bound-bullet blt))
		   live-bullets)
		  (prune-dead-enemies)
		  (run-tasks)
		  (tick-misc-ents)
		  (tick-particles)
		  (process-collisions))
		(raylib:begin-drawing)
		(render-all textures fonts)
		(raylib:end-drawing)
		(unless paused
		  (set! frames (fx1+ frames)))
		(set! true-frames (fx1+ true-frames))
		(loop)))
	(unload-audio)
	(unload-fonts fonts)
	(unload-textures textures)
	(unload-sfx)
	(raylib:close-window)))

(scheme-start (lambda _ (main)))
