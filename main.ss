#!chezscheme
;; Copyright (C) 2025 Vincent Lee; GPL-3.0-or-later
;; this is needed separately to bring add-prefix into scope when treating this
;; file as a r6rs Program
(import (chezscheme))
(import (add-prefix (raylib) raylib:)
		(coro) (geom) (funcutils) (config))
(define key-space 32)
(define key-escape 256)
(define key-f1 290)
(define key-f2 291)
(define key-f3 292)
(define key-left-shift 340)
(define key-right 262)
(define key-left 263)
(define key-down 264)
(define key-up 265)
(define key-comma 44)
(define key-period 46)
(define key-a 65)
(define key-d 68)
(define key-f 70)
(define key-g 71)
(define key-r 82)
(define key-s 83)
(define key-x 88)
(define key-y 89)
(define key-z 90)
(define key-left-bracket 91)
(define key-right-bracket 93)
(define pi 3.141592)
(define tau 6.28318)
(alias vnth vector-ref)
(alias vlen vector-length)
(alias roll pseudo-random-generator-next!) ;; convenience alias
(define (vrand v rng)
  (vector-ref v (roll rng (vector-length v))))
(define-enumeration miscenttype
  (mainshot needle point life-frag big-piv life bomb-frag small-piv bomb)
  make-miscent-type-set)
(define-enumeration particletype
  (cancel itemvalue enmdeath graze spellbonus maple-grayscale maple
		  circle-hint)
  make-particletype-set)
(define-enumeration bltflag
  (uncancelable ;; cannot be cancelled by bombs or other standard cancels
   noprune ;; do not prune the bullet when it travels out of bounds
   )
  bltflags)
(define empty-bltflags (bltflags))
(define-enumeration enmflag
  (invincible nocollide)
  enmflags)
(define empty-enmflags (enmflags))

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

(define (round-score to-add) ;; to-add can be a flonum
  (define rounded (eround to-add))
  (define rem (remainder rounded 10))
  (define res (* 10 (quotient rounded 10)))
  (if (>= rem 5)
	  (+ 10 res)
	  res))

(define config #f)

(define-syntax interval-loop
  (syntax-rules ()
	[(_ intvl b ...)
	 (let loop ()
	   b ...
	   (wait intvl)
	   (loop))]))
(define-syntax interval-loop-while
  (syntax-rules ()
	[(_ intvl cond b ...)
	 (let loop ()
	   (when cond
		 b ...
		 (wait intvl)
		 (loop)))]))
(define-syntax interval-loop-waitfirst
  (syntax-rules ()
	[(_ intvl b ...)
	 (let loop ()
	   (wait intvl)
	   b ...
	   (loop))]))
(define-syntax loop-forever
  (syntax-rules ()
	[(_ b ...)
	 (interval-loop 1 b ...)]))
(define-syntax loop-until
  (syntax-rules ()
	[(_ condition b ...)
	 (let loop ()
	   (unless condition
		 b ...
		 (yield)
		 (loop)))]))

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
   timeout timeoutwarn item damage0 damage1
   laser damageresist)
  (sealed #t))
(define sounds #f)
(define (each-sound proc)
  (define rtd (record-type-descriptor sebundle))
  (define num-sounds (vlen (record-type-field-names rtd)))
  (when sounds
	(do [(i 0 (1+ i))]
		[(>= i num-sounds)]
	  (proc ((record-accessor rtd i) sounds)))))
(define (update-sound-volumes)
  (let ([vol-flt (inexact (/ (cdr (assq 'sfx-vol config)) 100.0))])
	(each-sound (lambda (sound) (raylib:set-sound-volume sound vol-flt)))))
(define (load-sfx)
  (set! sounds
		(apply
		 make-sebundle
		 (map (lambda (file) (raylib:load-sound (string-append "assets/sfx/" file)))
			  '("se_cardget.wav" "se_cat00.wav"
				"se_ch00.wav" "se_ch02.wav"
				"se_enep00.wav" "se_enep01.wav"
				"se_pldead00.wav" "se_plst00.wav"
				"se_tan00.wav" "se_tan01.wav" "se_tan02.wav"
				"se_extend.wav" "se_graze.wav" "se_kira00.wav"
				"se_power1.wav" "se_power2.wav"
				"se_pause.wav" "se_select00.wav"
				"se_timeout.wav" "se_timeout2.wav"
				"se_item00.wav" "se_damage00.wav" "se_damage01.wav"
				"se_old_lazer01.wav" "se_nodamage.wav"))))
  (update-sound-volumes))
(define (unload-sfx)
  (each-sound raylib:unload-sound)
  (set! sounds #f))
(define (increase-sound-volume)
  (define pair (assq 'sfx-vol config))
  (when (< (cdr pair) 100)
	(set-cdr! pair (+ (cdr pair) 5))
	(save-config config)
	(update-sound-volumes)))
(define (decrease-sound-volume)
  (define pair (assq 'sfx-vol config))
  (when (> (cdr pair) 0)
	(set-cdr! pair (- (cdr pair) 5))
	(save-config config)
	(update-sound-volumes)))

(define-record-type txbundle
  (fields
   reimu
   enemy1
   hud
   misc item
   bullet1 bullet2 bullet3 bullet4 bullet5 bullet6
   bullet-music
   bg1 bg2 bg3 bg4
   bulletcancel hint
   bullet-ball-huge
   laser1 laser2 laser3 laser4
   magicircle boss boss-flip boss-ui)
  (sealed #t))
(define (load-textures)
  (define (ltex file)
	(raylib:load-texture (string-append "assets/img/" file)))  
  (apply make-txbundle
		 (map ltex '("reimu.png"
					 "enemy1.png"
					 "ui_bg.png"
					 "misc.png" "item.png"
					 "bullet1.png" "bullet2.png" "bullet3.png"
					 "bullet4.png" "bullet5.png" "bullet6.png"
					 "bullet_music.png"
					 "background_1.png" "background_2.png"
					 "background_3.png" "background_4.png"
					 "etbreak.png" "hint.png"
					 "bullet_ball_huge.png"
					 "laser1.png" "laser2.png" "laser3.png" "laser4.png"
					 "eff_magicsquare.png" "boss.png" "boss_rot.png"
					 "boss_ui.png"))))

(define (unload-textures textures)
  (define rtd (record-type-descriptor txbundle))
  (define num-textures (vlen (record-type-field-names rtd)))
  (do [(i 0 (1+ i))]
	  [(>= i num-textures)]
	(raylib:unload-texture ((record-accessor rtd i) textures))))

(define-record-type fontbundle
  (fields
   bubblegum
   cabin
   sharetechmono)
  (sealed #t))
(define (load-fonts)
  (make-fontbundle
   (raylib:load-font "assets/font/BubblegumSans-Regular.ttf")
   (raylib:load-font "assets/font/Cabin-Regular.ttf")
   (raylib:load-font "assets/font/ShareTechMono-Regular.ttf")))
(define (unload-fonts fonts)
  (define rtd (record-type-descriptor fontbundle))
  (define num-fonts (vlen (record-type-field-names rtd)))
  (do [(i 0 (1+ i))]
	  [(>= i num-fonts)]
	(raylib:unload-font ((record-accessor rtd i) fonts))))

(define-record-type sprite-descriptor
  (fields
   tx-accessor
   bounds
   center-shift)
  (sealed #t))

(define basic-colors '(red magenta blue cyan green yellow orange white))
(define sprite-data
  (let ()
  (define ret (make-hashtable symbol-hash eq?))
  (define shift8 (vec2 -8.0 -8.0))
  (define shift16 (vec2 -16.0 -16.0))
  (define shift24 (vec2 -24.0 -24.0))
  (define shift32 (vec2 -32.0 -32.0))
  (define (make type accessor x y width height shift)
	(symbol-hashtable-set!
	 ret type
	 (make-sprite-descriptor accessor
							 (make-rectangle
							  (inexact x) (inexact y)
							  (inexact width) (inexact height))
							 shift)))
  (define (make-vertical-group prefix elems accessor x y width height shift)
	(for-each-indexed
	 (lambda (i elem)
	   (define type
		 (string->symbol (string-append
						  (symbol->string prefix)
						  "-"
						  (symbol->string elem))))
	   (make type accessor x (+ y (* i height)) width height shift))
	 elems))
  ;; skip double the space between each member of the group
  (define (make-vertical-group-skip prefix elems accessor x y width height shift)
	(for-each-indexed
	 (lambda (i elem)
	   (define type
		 (string->symbol (string-append
						  (symbol->string prefix)
						  "-"
						  (symbol->string elem))))
	   (make type accessor x (+ y (* 2 i height)) width height shift))
	 elems))
  ;; lasers
  (for-each-indexed
   (lambda (i color)
	 (define type
	   (string->symbol (string-append
						"fixed-laser-"
						(symbol->string color))))
	 (make type txbundle-laser4 0 (+ (* 2 i 16) 3) 256 10 v2zero))
   basic-colors)
  ;; bullets
  (make-vertical-group-skip
   'pellet basic-colors
   txbundle-bullet2 176 0 16 16 shift8)
  (make-vertical-group-skip
   'rice basic-colors
   txbundle-bullet2 160 0 16 16 shift8)
  (make-vertical-group-skip
   'small-star basic-colors
   txbundle-bullet2 96 0 16 16 shift8)
  (make-vertical-group
   'big-star basic-colors
   txbundle-bullet2 224 0 32 32 shift16)
  (make-vertical-group
   'preimg basic-colors
   txbundle-bullet1 80 0 32 32 shift16)
  (make-vertical-group
   'butterfly basic-colors
   txbundle-bullet1 112 0 32 32 shift16)
  (make-vertical-group
   'ellipse basic-colors
   txbundle-bullet1 224 0 32 32 shift16)
  (make-vertical-group-skip
   'arrowhead basic-colors
   txbundle-bullet1 0 0 16 16 shift8)
  (make-vertical-group-skip
   'amulet basic-colors
   txbundle-bullet1 152 0 16 16 shift8)
  (make-vertical-group
   'small-ball basic-colors
   txbundle-bullet1 176 0 32 32 shift16)
  (make-vertical-group
   'medium-ball basic-colors
   txbundle-bullet2 192 0 32 32 shift16)
  (make-vertical-group
   'ice-shard basic-colors
   txbundle-bullet2 128 0 16 16 shift8)
  (make-vertical-group
   'knife basic-colors
   txbundle-bullet3 0 0 32 32 (vec2 -15.0 -16.0))
  (make-vertical-group-skip
   'bacteria basic-colors
   txbundle-bullet3 48 0 16 16 shift8)
  (make-vertical-group-skip
   'kunai basic-colors
   txbundle-bullet3 80 0 16 16 shift8)
  (make-vertical-group
   'droplet basic-colors
   txbundle-bullet3 112 0 16 16 shift8)
  (make-vertical-group
   'heart basic-colors
   txbundle-bullet4 128 0 32 32 shift16) ;; TODO this is too big, needs scaling
  (make-vertical-group
   'arrow basic-colors
   txbundle-bullet4 96 0 32 32 (vec2 -24.0 -16.0)) ;; also too big
  (make-vertical-group
   'rest basic-colors
   txbundle-bullet6 192 0 32 32 shift16)

  (make 'glow-orb-red txbundle-bullet5 0 0 64 64 shift32)
  (make 'glow-orb-green txbundle-bullet5 0 64 64 64 shift32)
  (make 'glow-orb-magenta txbundle-bullet5 64 0 64 64 shift32)
  (make 'glow-orb-yellow txbundle-bullet5 64 64 64 64 shift32)
  (make 'glow-orb-blue txbundle-bullet5 0 128 0 64 shift32)
  (make 'glow-orb-orange txbundle-bullet5 128 64 64 64 shift32)
  (make 'glow-orb-cyan txbundle-bullet5 192 0 64 64 shift32)
  (make 'glow-orb-white txbundle-bullet5 192 64 64 64 shift32)

  (make 'bubble-red txbundle-bullet-ball-huge 0 0 64 64 shift32)
  (make 'bubble-green txbundle-bullet-ball-huge 0 64 64 64 shift32)
  (make 'bubble-magenta txbundle-bullet-ball-huge 64 0 64 64 shift32)
  (make 'bubble-yellow txbundle-bullet-ball-huge 64 64 64 64 shift32)
  (make 'bubble-blue txbundle-bullet-ball-huge 128 0 64 64 shift32)
  (make 'bubble-orange txbundle-bullet-ball-huge 128 64 64 64 shift32)
  (make 'bubble-cyan txbundle-bullet-ball-huge 192 0 64 64 shift32)
  (make 'bubble-white txbundle-bullet-ball-huge 192 64 64 64 shift32)

  (for-each-indexed
   (lambda (i color)
	 (define shift (vec2 -30.0 -16.0))
	 (do [(j 0 (add1 j))]
		 [(> j 2)]
	   (make (string->symbol
			  (string-append "music-" (symbol->string color) (number->string j)))
		 txbundle-bullet-music (* 60 i) (* 32 j) 60 32 shift)))
   basic-colors)

  ;; small fairies
  (do [(i 0 (add1 i))]
	  [(>= i 12)]
	;; this is gross but not sure what names would be less bad than
	;; just straight up integers. See rendering code for what each one means.
	(let* ([snum (number->string i)]
		   [red (->> snum
					 (string-append "red-fairy")
					 string->symbol)]
		   [green (->> snum
					   (string-append "green-fairy")
					   string->symbol)]
		   [blue (->> snum
					  (string-append "blue-fairy")
					  string->symbol)]
		   [yellow (->> snum
						(string-append "yellow-fairy")
						string->symbol)])
	  (make red txbundle-enemy1 (* 32 i) 384 32 32 shift16)
	  (make green txbundle-enemy1 (* 32 i) 416 32 32 shift16)
	  (make blue txbundle-enemy1 (* 32 i) 448 32 32 shift16)
	  (make yellow txbundle-enemy1 (* 32 i) 480 32 32 shift16)))

  (for-each
   (lambda (l)
	 (define type (car l))
	 (define start-x (cadr l))
	 (define start-y (caddr l))
	 (do [(i 0 (add1 i))]
		 [(>= i 12)]
	   ;; likewise gross
	   (let-values ([(name) (->> (number->string i)
								 (string-append type)
								 string->symbol)]
					[(row col) (div-and-mod i 4)])
		 (make name txbundle-enemy1
			   (+ start-x (* 48 col))
			   (+ start-y (* 48 row))
			   48 48 shift24))))
   '(("medium-red-fairy" 320 0)
	 ("medium-blue-fairy" 320 144)))

  (do [(i 0 (add1 i))]
	  [(>= i 12)]
	;; likewise gross
	(let-values ([(name) (->> (number->string i)
							  (string-append "big-fairy")
							  string->symbol)]
				 [(row col) (div-and-mod i 4)])
	  (make name txbundle-enemy1
			(* 64 col)
			(+ 192 (* 64 row))
			64 64 shift32)))

  (make 'red-yinyang txbundle-enemy1 192 64 32 32 shift16)
  (make 'green-yinyang txbundle-enemy1 224 64 32 32 shift16)
  (make 'blue-yinyang txbundle-enemy1 256 64 32 32 shift16)
  (make 'magenta-yinyang txbundle-enemy1 288 64 32 32 shift16)
  (make 'red-yinyang-outer txbundle-enemy1 192 96 32 32 shift16)
  (make 'green-yinyang-outer txbundle-enemy1 224 96 32 32 shift16)
  (make 'blue-yinyang-outer txbundle-enemy1 256 96 32 32 shift16)
  (make 'magenta-yinyang-outer txbundle-enemy1 288 96 32 32 shift16)

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
  (make 'option txbundle-reimu 64 144 16 16 shift8)
  (make 'needle txbundle-reimu 64 176 64 16 (vec2 -54.0 -8.0))
  (make 'life-third txbundle-hint 288 16 16 16 v2zero)
  (make 'life-two-thirds txbundle-hint 320 16 16 16 v2zero)
  (make 'life-full txbundle-hint 304 0 16 16 v2zero)
  (make 'bomb-third txbundle-hint 288 32 16 16 v2zero)
  (make 'bomb-two-thirds txbundle-hint 320 32 16 16 v2zero)
  (make 'bomb-full txbundle-hint 336 0 16 16 v2zero)
  (make 'magicircle txbundle-magicircle 0 0 256 256 (vec2 -128.0 -128.0))
  (make 'enemy-indicator txbundle-misc 128 72 48 16 (vec2 -24.0 0.0))
  (make 'maple-grayscale txbundle-misc 0 0 32 32 shift16)
  (make 'maple txbundle-misc 0 32 32 32 shift16)
  ret))

(define (draw-laser-sprite textures sprite-id x y length radius
						   rotation shine-sprite)
  (define data (symbol-hashtable-ref sprite-data sprite-id #f))
  (raylib:with-matrix
   (raylib:translatef x y 0.0)
   (raylib:rotatef (todeg rotation) 0.0 0.0 1.0)
   ;; rotate about the left edge, halfway down
   (raylib:translatef 0.0 (fl- radius) 0.0)
   (raylib:draw-texture-pro
	((sprite-descriptor-tx-accessor data) textures)
	(sprite-descriptor-bounds data)
	(make-rectangle 0.0 0.0 length (fl* radius 2.0))
	v2zero 0.0 -1))
  (when shine-sprite
	(draw-sprite-with-rotation textures shine-sprite
							   (mod (* frames 11.0) 360.0) x y -1)))

(define (draw-sprite textures sprite-id x y color)
  (define data (symbol-hashtable-ref sprite-data sprite-id #f))
  (raylib:draw-texture-rec
   ((sprite-descriptor-tx-accessor data) textures)
   (sprite-descriptor-bounds data)
   (v2+ (vec2 x y) (sprite-descriptor-center-shift data))
   color))

(define (draw-sprite-mirror-x textures sprite-id x y color)
  (define data (symbol-hashtable-ref sprite-data sprite-id #f))
  (define bounds (sprite-descriptor-bounds data))
  (raylib:draw-texture-rec
   ((sprite-descriptor-tx-accessor data) textures)
   (make-rectangle (rectangle-x bounds) (rectangle-y bounds)
				   (- (rectangle-width bounds)) (rectangle-height bounds))
   (v2+ (vec2 x y) (sprite-descriptor-center-shift data))
   color))

(define (draw-sprite-pro textures sprite-id dest color)
  (define data (symbol-hashtable-ref sprite-data sprite-id #f))
  (raylib:draw-texture-pro
   ((sprite-descriptor-tx-accessor data) textures)
   (sprite-descriptor-bounds data)
   dest
   v2zero 0.0 color))

(define (draw-sprite-with-rotation textures sprite-id rotation x y color)
  (define data (symbol-hashtable-ref sprite-data sprite-id #f))
  (define center-shift (sprite-descriptor-center-shift data))
  (raylib:with-matrix
   (raylib:translatef x y 0.0)
   (raylib:rotatef (inexact rotation) 0.0 0.0 1.0)
   (raylib:translatef (v2x center-shift) (v2y center-shift) 0.0)
   (raylib:draw-texture-rec
	((sprite-descriptor-tx-accessor data) textures)
	(sprite-descriptor-bounds data)
	v2zero
	color)))

(define (draw-sprite-pro-with-rotation textures sprite-id rotation dest color)
  (define data (symbol-hashtable-ref sprite-data sprite-id #f))
  (raylib:draw-texture-pro
   ((sprite-descriptor-tx-accessor data) textures)
   (sprite-descriptor-bounds data) dest
   (vec2 (/ (rectangle-width dest) 2.0) (/ (rectangle-height dest) 2.0))
   rotation color))

(define (draw-sprite-with-scale-rotation textures sprite-id rotation scale x y color)
  (define data (symbol-hashtable-ref sprite-data sprite-id #f))
  (define center-shift (sprite-descriptor-center-shift data))
  (raylib:with-matrix
   (raylib:translatef x y 0.0)
   (raylib:rotatef (inexact rotation) 0.0 0.0 1.0)
   (raylib:scalef scale scale 1.0)
   (raylib:translatef (+ (v2x center-shift)) (+ (v2y center-shift)) 0.0)
   (raylib:draw-texture-rec
	((sprite-descriptor-tx-accessor data) textures)
	(sprite-descriptor-bounds data)
	v2zero
	color))
  )

(define (packcolor r g b a)
  (bitwise-ior (bitwise-arithmetic-shift-left r 24)
			   (bitwise-arithmetic-shift-left g 16)
			   (bitwise-arithmetic-shift-left b 8)
			   a))
(define red (packcolor 230 41 55 255))
(define green (packcolor 0 228 48 255))
(define invincible-flash (packcolor 64 64 255 255))
(define damage-flash (packcolor 255 64 64 255))

;; [31-416] x bounds of playfield in the hud texture
;; [15-463] y bounds of playfield in the hud texture
;; idea is to have logical game x in [-192, 192] (192 is (416-31)/2, roughly), and y in [0, 448] 
;; logical game 0, 0 is at gl (31+(416-31)/2), 15
;; offset logical coords by 223, 15 to get to GL coords for render
(define +playfield-render-offset-x+ 223)
(define +playfield-render-offset-y+ 15)
(define +playfield-render-offset+ (vec2 (inexact +playfield-render-offset-x+)
										(inexact +playfield-render-offset-y+)))
(define +playfield-min-x+ -192)
(define +playfield-min-y+ 0)
(define +playfield-max-x+ 192)
(define +playfield-max-y+ 448)
(define +playfield-min-render-x+ (+ +playfield-min-x+ +playfield-render-offset-x+))
(define +playfield-max-render-x+ (+ +playfield-max-x+ +playfield-render-offset-x+))
(define +playfield-min-render-y+ (+ +playfield-min-y+ +playfield-render-offset-y+))
(define +playfield-max-render-y+ (+ +playfield-max-y+ +playfield-render-offset-y+))
(define +playfield-width+ (- +playfield-max-x+ +playfield-min-x+))
(define +playfield-height+ (- +playfield-max-y+ +playfield-min-y+))
(define +poc-y+ 160)
(define +oob-bullet-despawn-fuzz+ 80)

;; Number of frames the current stage has been running. Does not increment when paused
(define frames 0)
(define frame-save 0)
(define frame-save-diff 0)
(define current-chapter 0) ;; informational/debug only
;; Always increments by one per frame no matter what. Should not be used often.
(define true-frames 0)
;; The value of frames when the shot button was pressed down.
;; -1 if shot is not currently held
(define start-shot-frames -1)
(define iframes 0) ;; Remaining frames of invincibility
(define respawning 0) ;; Nonzero if going through the respawn animation
(define +respawning-max+ 60)
(define show-hitboxes #f)
(define bombing 0) ;; Nonzero if a bomb is in progress
(define +bombing-max+ 180)
(define +bomb-initial-phase-length+ 60)
(define +bomb-noninitial-phase-length+ (- +bombing-max+ +bomb-initial-phase-length+))
(define bomb-sweep-x-left 0.0)
(define bomb-sweep-x-right 0.0)
(define bomb-sweep-y-up 0.0)
(define bomb-sweep-y-down 0.0)
(define initial-bomb-sweep-x-left 0.0)
(define initial-bomb-sweep-x-right 0.0)
(define initial-bomb-sweep-y-up 0.0)
(define initial-bomb-sweep-y-down 0.0)
(define +deathbomb-time+ 10)
;; If positive, player is going to be killed in N frames, unless a bomb is used.
(define death-timer 0)
(define force-invincible #f)
(define graze 0)
(define paused #f)
(define graze-radius 22.0)
(define hit-radius 3.0)
(define vacuum-radius-unfocused 25.0)
(define vacuum-radius-focused 55.0)
(define player-x 0.0)
;; Increments away from 0 whenever horizontal movement happens, returns to 0 otherwise
(define player-dx-render 0)
(define +initial-player-y+ (- +playfield-max-y+ 20.0))
(define player-y +initial-player-y+)
(define focused-immediate #f)
;; increases when focus is held, decreases when it isn't
;; used for things that smooth over several frames
(define focus-frames 0)
(define +max-focus-frames+ 10)
(define option-xs (make-vector 4 0.0))
(define option-ys (make-vector 4 0.0))
(define item-value 10000)
(define current-score 0)
;; fragments are stored as Scheme's fractional data types
(define life-stock 2)
(define bomb-stock 3)
(define game-rng (make-pseudo-random-generator))
(define visual-rng (make-pseudo-random-generator))
(define (centered-roll rng radius)
  (- (* (roll rng) 2 radius)
	 radius))
(define chapter-select 0)
(define spline-editor-positions '#())
(define spline-editor-selected-position 0)

(define (truncate-to-whole-spline v)
  (let*-values ([(quot rem) (div-and-mod (sub1 (vlen v)) 3)])
	(if (fxzero? rem)
		;; can directly pass
		v
		;; need to truncate it to the nearest complete segment
		(vector-truncate v (add1 (* 3 quot))))))

(define (facing-player x y)
  (flatan (fl- player-y y) (fl- player-x x)))

(define (dist-away enm facing dist)
  (values (fl+ (enm-x enm) (fl* dist (flcos facing)))
		  (fl+ (enm-y enm) (fl* dist (flsin facing)))))

(define (player-invincible?)
  (fxpositive? iframes))

(define ojamajo-carnival #f)
(define (load-audio)
  (raylib:init-audio-device)
  (set! ojamajo-carnival (raylib:load-music-stream "assets/bgm/ojamajo_carnival.wav"))
  (raylib:set-music-volume ojamajo-carnival (inexact (/ (cdr (assq 'music-vol config)) 100.0))))
(define (unload-audio)
  (raylib:unload-music-stream ojamajo-carnival)
  (set! ojamajo-carnival #f)
  (raylib:close-audio-device))

(define (increase-music-volume)
  (define pair (assq 'music-vol config))
  (when (< (cdr pair) 100)
	(set-cdr! pair (+ (cdr pair) 5))
	(save-config config)
	(raylib:set-music-volume ojamajo-carnival (inexact (/ (cdr pair) 100.0)))))
(define (decrease-music-volume)
  (define pair (assq 'music-vol config))
  (when (> (cdr pair) 0)
	(set-cdr! pair (- (cdr pair) 5))
	(save-config config)
	(raylib:set-music-volume ojamajo-carnival (inexact (/ (cdr pair) 100.0)))))

(define next-bullet-id 1)
(define (get-next-bullet-id)
  (define res next-bullet-id)
  (set! next-bullet-id (fx1+ next-bullet-id))
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

(define-record-type laser
  (parent bullet)
  (fields
   (mutable length)
   radius ;; aka half-thickness
   despawn-time ;; how long the laser despawn animation takes
   (mutable last-grazed-at)
   (mutable start-despawning-at)))

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

(define bullet-types
  (let* ([ret (make-hashtable symbol-hash eq?)]
		 [basic-colors '(red magenta blue cyan green yellow orange white)]
		 [preimg-sprite-mapping
		  (map (lambda (color)
				 (cons color (string->symbol (string-append "preimg-"
															(symbol->string color)))))
			   basic-colors)]
		 [make-family
		  (lambda (family colors hit-radius)
			(for-each (lambda (color)
						(define type
						  (string->symbol (string-append
										   (symbol->string family)
										   "-"
										   (symbol->string color))))
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
	(make-family 'small-star basic-colors 3.7)
	(make-family 'big-star basic-colors 5.5)
	(make-family 'rice basic-colors 2.0)
	(make-family 'pellet basic-colors 2.0)
	(make-family 'butterfly basic-colors 3.7)
	(make-family 'ellipse basic-colors 4.0)
	(make-family 'arrowhead basic-colors 3.0)
	(make-family 'amulet basic-colors 3.1)
	(make-family 'small-ball basic-colors 3.0)
	(make-family 'medium-ball basic-colors 9.0)
	(make-family 'ice-shard basic-colors 2.5)
	(make-family 'fixed-laser basic-colors 0.0)
	(make-family 'rest basic-colors 3.0)
	(make-family 'music basic-colors 3.0)
	(make-family 'knife basic-colors 4.0)
	(make-family 'bacteria basic-colors 2.0)
	(make-family 'kunai basic-colors 2.5)
	(make-family 'droplet basic-colors 2.0)
	(make-family 'heart basic-colors 6.0)
	(make-family 'arrow basic-colors 3.0)
	(make-family 'glow-orb basic-colors 9.0)
	(for-each (lambda (color)
				(define type
				  (string->symbol (string-append
								   "bubble-"
								   (symbol->string color))))
				(symbol-hashtable-set!
				 ret
				 type
				 (make-blttype type 'bubble type 2.0 32.0 16.0)))
			  basic-colors)
	ret))

(define (bullet-active? blt)
  ;; whether the bullet participates in gameplay
  (and (fxnonnegative? (bullet-livetime blt))
	   (or (not (laser? blt))
		   (not (laser-start-despawning-at blt)))))

(define (bullet-hasflag? blt flag)
  (enum-set-member? flag (bullet-flags blt)))

(define (bullet-addflags blt flags)
  (bullet-flags-set! blt (enum-set-union (bullet-flags blt) flags)))

(define (bullet-clrflags blt flags)
  (bullet-flags-set! blt (enum-set-difference (bullet-flags blt) flags)))

(define live-bullets (make-vector 4096 #f))

(define (spawn-bullet type x y delay control-function)
  (let ([idx (vector-index #f live-bullets)])
	(unless idx
	  (error 'spawn-bullet "No more open bullet slots"))
	(let ([blt (make-bullet (get-next-bullet-id)
							type x y 0.0 #f (- delay) (- delay)
							empty-bltflags)])
	  (vector-set! live-bullets idx blt)
	  (spawn-task "bullet"
				  (lambda (task)
					(do [(i 0 (fx1+ i))]
						[(fx> i delay)]
					  (bullet-livetime-set! blt (fx1+ (bullet-livetime blt)))
					  (yield))
					(control-function blt))
				  (thunk (eq? blt (vnth live-bullets idx))))
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
	  (spawn-task "laser"
				  (lambda (task)
					(do [(i 0 (fx1+ i))]
						[(fx> i delay)]
					  (bullet-livetime-set! blt (fx1+ (bullet-livetime blt)))
					  (yield))
					(control-function blt)
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
		  (spawn-particle (make-particle
						   (particletype cancel)
						   (bullet-x bullet) (bullet-y bullet)
						   23 0 #f))
		  (delete-bullet bullet)))))

(define (spawn-drop-with-autocollect x y drop)
  (define ent (spawn-misc-ent drop x y -3.0 0.1))
  (spawn-task "delayed autocollect"
			  (lambda (task) (wait 45) (miscent-autocollect-set! ent #t))
			  (constantly #t)))

(define cancel-bullet-with-drop
  (case-lambda
	([bullet drop] (cancel-bullet-with-drop bullet drop #f))
	([bullet drop force]
	 (when (cancel-bullet bullet force)
	   (spawn-drop-with-autocollect (bullet-x bullet) (bullet-y bullet) drop)))))

(define (cancel-all force)
  (vector-for-each-truthy
   (lambda (blt)
	 (cancel-bullet-with-drop blt 'small-piv force))
   live-bullets))

(define (despawn-out-of-bound-bullet bullet)
  (let ([x (bullet-x bullet)]
		[y (bullet-y bullet)])
	(when (and
		   (not (bullet-hasflag? bullet (bltflag noprune)))
		   (or (> x (+ +playfield-max-x+ +oob-bullet-despawn-fuzz+))
			   (< x (- +playfield-min-x+ +oob-bullet-despawn-fuzz+))
			   (< y (- +playfield-min-y+ +oob-bullet-despawn-fuzz+))
			   (> y (+ +playfield-max-y+ +oob-bullet-despawn-fuzz+))))
	  (delete-bullet bullet))))

(define (bullet-family type)
  (define bt (symbol-hashtable-ref bullet-types type #f))
  (blttype-family bt))

(define (bullet-hit-radius type)
  (define bt (symbol-hashtable-ref bullet-types type #f))
  (blttype-hit-radius bt))

(define (draw-lasers textures sorted-bullets)
  (define (each bullet)
	(let* ([render-x (+ (bullet-x bullet) +playfield-render-offset-x+)]
		   [render-y (+ (bullet-y bullet) +playfield-render-offset-y+)]
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
							  (blttype-preimg-sprite bt)))))))
  (vector-for-each-truthy each sorted-bullets))
  

(define (draw-bullets textures sorted-bullets)
  (define (each bullet)
	(let* ([render-x (+ (bullet-x bullet) +playfield-render-offset-x+)]
		   [render-y (+ (bullet-y bullet) +playfield-render-offset-y+)]
		   [type (bullet-type bullet)]
		   [bt (symbol-hashtable-ref bullet-types type #f)]
		   [livetime (bullet-livetime bullet)])
	  (if (and (not (eq? 'fixed-laser (bullet-family type)))
			   (fxnegative? livetime))
		  (let* ([preimg-begin (blttype-preimg-begin-size bt)]
				 [preimg-end (blttype-preimg-end-size bt)]
				 ;; reversed because the factor is negative
				 [radius (lerp preimg-end preimg-begin
							   (/ livetime (bullet-initial-livetime bullet)))])
			(draw-sprite-pro
			 textures (blttype-preimg-sprite bt)
			 (make-rectangle (- render-x radius) (- render-y radius)
							 (* 2.0 radius) (* 2.0 radius))
			 -1))
		(let ()
		  (case (bullet-family type)
			;; basic
			([pellet small-ball medium-ball glow-orb]
			 (draw-sprite textures type render-x render-y #xffffffff))
			;; aimed in direction of movement
			([butterfly ellipse arrowhead amulet ice-shard rice
						rest knife bacteria kunai droplet heart arrow]
			 (draw-sprite-with-rotation textures type
										(todeg (bullet-facing bullet))
										render-x render-y -1))
			;; spinny
			([small-star big-star]
			 (draw-sprite-with-rotation textures type (fxmod (fx* frames 5) 360)
										render-x render-y -1))
			([bubble]
			 (draw-sprite-with-rotation textures type (fxmod (fx* frames 8) 360)
										render-x render-y -1))
			([music]
			 (let ([sprite
					(string->symbol (string-append (symbol->string type)
												   (number->string
													(fxmod (fx/ frames 10) 3))))])
			   (draw-sprite-with-rotation textures sprite 90.0
										  render-x render-y -1))))
		  (when show-hitboxes
			(raylib:draw-circle-v render-x render-y (bullet-hit-radius type)
								  red))))))
  (vector-for-each-truthy each sorted-bullets))

(define (draw-bomb textures)
  (raylib:draw-rectangle-gradient-h
   (eround (+ +playfield-render-offset-x+ (- bomb-sweep-x-left 290)))
   (+ +playfield-render-offset-y+ +playfield-min-y+)
   290
   +playfield-height+
   #x8a2be200
   #x8a2be2be)
  (raylib:draw-rectangle-gradient-v
   (+ +playfield-render-offset-x+ +playfield-min-x+)
   (eround (+ +playfield-render-offset-y+ (- bomb-sweep-y-up 365)))
   +playfield-width+
   365
   #xdc143c00
   #xdc143cbe)
  (raylib:draw-rectangle-gradient-h
   (eround (+ +playfield-render-offset-x+ bomb-sweep-x-right))
   (+ +playfield-render-offset-y+ +playfield-min-y+)
   290
   +playfield-height+
   #x8a2be2be
   #x8a2be200)
  (raylib:draw-rectangle-gradient-v
   (+ +playfield-render-offset-x+ +playfield-min-x+)
   (eround (+ +playfield-render-offset-y+ bomb-sweep-y-down))
   +playfield-width+
   365
   #xdc143cbe
   #xdc143c00))

(define-record-type fan-builder
  (fields
   (mutable rows)
   (mutable row-width)
   (mutable min-speed)
   (mutable max-speed)
   (mutable global-angle)
   (mutable local-angle)
   (mutable aimed-at-player))
  (sealed #t))
(define (fb)
  (make-fan-builder 0 0 0.0 0.0 0.0 0.0 #t))
(define (fbabsolute-aim fb)
  (fan-builder-aimed-at-player-set! fb #f)
  fb)
(define fbcounts
  (case-lambda
	[(fb row-width)
	 (fbcounts fb row-width 1)]
	[(fb row-width rows)
	 (assert (and (positive? rows) (positive? row-width)))
	 (fan-builder-rows-set! fb rows)
	 (fan-builder-row-width-set! fb row-width)
	 fb]))
(define fbspeed
  (case-lambda
	[(fb speed)
	 (fbspeed fb speed speed)]
	[(fb min max)
	 (fan-builder-min-speed-set! fb min)
	 (fan-builder-max-speed-set! fb max)
	 fb]))
(define (fbang fb global local)
  (fan-builder-global-angle-set! fb (torad global))
  (fan-builder-local-angle-set! fb (torad local))
  fb)
(define (fbshoot fb x y consume)
  (define rows (fan-builder-rows fb))
  (define row-width (fan-builder-row-width fb))
  (define min-speed (fan-builder-min-speed fb))
  (define max-speed (fan-builder-max-speed fb))
  (define base-angle
	(if (fan-builder-aimed-at-player fb)
		(+ (fan-builder-global-angle fb)
		   (facing-player x y))
		(fan-builder-global-angle fb)))
  (define local-angle (fan-builder-local-angle fb))
  (define starting-angle-offset
	(if (odd? row-width)
		(- (* (quotient row-width 2)
			  local-angle))
		(- (+ (/ local-angle 2.0)
			  (* (sub1 (/ row-width 2))
				 local-angle)))))
  (do [(row 0 (add1 row))] [(>= row rows)]
	(let ([speed (lerp min-speed max-speed (/ row rows))])
	  (do [(col 0 (add1 col))] [(>= col row-width)]
		(consume row col speed
 				 (+ base-angle starting-angle-offset
					(* col local-angle)))))))

;; helper for the common case of shooting a fanbuilder from the enemy position
;; if provided, bullet control function should take facing, speed, bullet
(define fbshootenm
  (case-lambda
	[(fb enm type delay sound)
	 (fbshootenm fb enm type delay sound linear-step-forever)]
	[(fb enm type delay sound control-function)
	 (fbshootez fb (enm-x enm) (enm-y enm) type delay sound control-function)]))

(define fbshootez
  (case-lambda
	[(fb x y type delay sound)
	 (fbshootez fb x y type delay sound linear-step-forever)]
	[(fb x y type delay sound control-function)
	 (fbshoot fb x y
	   (lambda (row col speed facing)
		 (when sound
		   (raylib:play-sound sound))
		 (-> (spawn-bullet type x y delay (curry control-function facing speed))
			 (bullet-facing-set! facing))))]))

(define-record-type circle-builder
  (fields
   (mutable layers)
   (mutable bullets-per-layer)
   (mutable min-speed)
   (mutable max-speed)
   (mutable global-angle)
   (mutable per-layer-angle)
   (mutable offset)
   (mutable aimed-at-player))
  (sealed #t))
(define (cb)
  (make-circle-builder 0 0 0.0 0.0 0.0 0.0 0.0 #t))

;; NB: Only works when used with cbshootenm/ez
(define (cboffset cb offset)
  (circle-builder-offset-set! cb offset)
  cb)
(define cbcount
  (case-lambda
	[(cb per-layer)
	 (cbcount cb per-layer 1)]
	[(cb per-layer layers)
	 (circle-builder-layers-set! cb layers)
	 (circle-builder-bullets-per-layer-set! cb per-layer)
	 cb]))
(define (cbabsolute-aim cb)
  (circle-builder-aimed-at-player-set! cb #f)
  cb)
;; If min-speed < max-speed, the layers are fired from inside to outside.
;; Otherwise, the layers are fired from outside to inside
(define cbspeed
  (case-lambda
	[(cb speed)
	 (cbspeed cb speed speed)]
	[(cb min-speed max-speed)
	 (circle-builder-min-speed-set! cb min-speed)
	 (circle-builder-max-speed-set! cb max-speed)
	 cb]))
(define cbang
  (case-lambda
	[(cb global)
	 (circle-builder-global-angle-set! cb (torad global))
	 cb]
	[(cb global per-layer)
	 (circle-builder-global-angle-set! cb (torad global))
	 (circle-builder-per-layer-angle-set! cb (torad per-layer))
	 cb]))
(define (cbshoot cb x y consume)
  (define layers (circle-builder-layers cb))
  (define per-layer (circle-builder-bullets-per-layer cb))
  (define min-speed (circle-builder-min-speed cb))
  (define max-speed (circle-builder-max-speed cb))
  (define per-layer-angle (circle-builder-per-layer-angle cb))
  (define initial-angle
	(if (circle-builder-aimed-at-player cb)
		(+ (circle-builder-global-angle cb)
		   (facing-player x y))
		(circle-builder-global-angle cb)))
  (define per-bullet-angle (/ tau per-layer))
  (do [(layer 0 (add1 layer))] [(>= layer layers)]
	(let ([speed (lerp min-speed max-speed (/ layer layers))]
		  [layer-angle (+ initial-angle (* layer per-layer-angle))])
	  (do [(in-layer 0 (add1 in-layer))] [(>= in-layer per-layer)]
		(consume layer in-layer speed
				 (+ layer-angle (* in-layer per-bullet-angle)))))))

;; helper for the common case of shooting a circlebuilder from the enemy position
;; with a fixed type and linear bullet motion
;; if provided, bullet control function should take facing, speed, bullet
(define cbshootenm
  (case-lambda
	[(cb enm type delay sound)
	 (cbshootenm cb enm type delay sound linear-step-forever)]
	[(cb enm type delay sound control-function)
	 (cbshootez cb (enm-x enm) (enm-y enm) type delay sound control-function)]))

(define cbshootez
  (case-lambda
	[(cb x y type delay sound)
	 (cbshootez cb x y type delay sound linear-step-forever)]
	[(cb x y type delay sound control-function)
	 (define offset (circle-builder-offset cb))
	 (cbshoot cb x y
			  (lambda (row col speed facing)
				(when sound
				  (raylib:play-sound sound))
				(-> (spawn-bullet type
								  (fl+ x (fl* offset (flcos facing)))
								  (fl+ y (fl* offset (flsin facing)))
								  delay (curry control-function facing speed))
					(bullet-facing-set! facing))))]))

(define +boss-lazy-spellcircle-context+ 30)
(define-record-type bossinfo
  (fields
   name
   name-color
   ;; holds last +boss-lazy-spellcircle-context+ frames of positions
   ;; from oldest to latest; used for the spell circle to trail behind the boss
   (mutable old-xs)
   (mutable old-ys)
   (mutable aura-active)
   (mutable active-spell-name)
   (mutable active-spell-bonus)
   ;; #t if the player bombed or died on the current attack
   (mutable active-attack-failed)
   (mutable remaining-timer) ;; frames remaining of time on this attack, 0 if none
   ;; total frames for this attack, 0 if none
   (mutable total-timer)
   ;; of the current attack, 0 if none, -1 if survival
   (mutable max-health)
   ;; immutable vector of dummy healthbars represented not-yet-declared attacks
   ;; used only for rendering
   (mutable dummy-healthbars))
  (sealed #t))
(define-record-type dummy-healthbar
  (fields
   width
   post-padding
   top-color
   bottom-color))

(define-enumeration enmtype
  (red-fairy green-fairy blue-fairy yellow-fairy
			 red-yinyang green-yinyang blue-yinyang magenta-yinyang
			 medium-red-fairy medium-blue-fairy
			 big-fairy boss-doremi boss-hazuki boss-aiko boss-onpu)
  make-enmtype-set)
(define-record-type enm
  (fields
   type
   (mutable x)
   (mutable y)
   (mutable ox)
   (mutable oy)
   (mutable health)
   ;; set to positive when damaged, decreases automatically every frame.
   (mutable damaged-recently)
   (mutable flags)
   ;; alist of (miscent type . count) to drop on death.
   (mutable drops)
   ;; optional nullary function to be called when the enemy health reaches zero
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
   (mutable extras)))
(define live-enm (make-vector 256 #f))
(define default-drop '((point . 1)))
(define five-point-items '((point . 5)))
(define ten-point '((point . 10)))

(define (enm-hasflag? enm flag)
  (enum-set-member? flag (enm-flags enm)))

(define (enm-addflags enm flags)
  (enm-flags-set! enm (enum-set-union (enm-flags enm) flags))
  enm)

(define (enm-clrflags enm flags)
  (enm-flags-set! enm (enum-set-difference (enm-flags enm) flags))
  enm)

(define (is-boss? enm)
  (member (enm-type enm) '(boss-doremi boss-hazuki boss-aiko boss-onpu)))
(define (first-boss)
  (vector-find
   (lambda (e) (and e (is-boss? e)))
   live-enm))

(define (calculate-spell-bonus bossinfo)
  (define grace-period 180)
  (define remaining-time (bossinfo-remaining-timer bossinfo))
  (define total-time (bossinfo-total-timer bossinfo))
  (define max-bonus (bossinfo-active-spell-bonus bossinfo))
  (cond
   [(bossinfo-active-attack-failed bossinfo)
	0]
   [(or (= -1 (bossinfo-max-health bossinfo))
		(fx<= (fx- total-time remaining-time) 180))
	max-bonus]
   [else
	(round-score (lerp (eround (/ max-bonus 2)) max-bonus
					   (/ remaining-time (- total-time grace-period))))]))

(define (fail-current-attack)
  (define (each enm)
	(when (is-boss? enm)
	  (bossinfo-active-attack-failed-set! (enm-extras enm) #t)))
  (vector-for-each-truthy each live-enm))

(define (pretick-enemies)
  (define (each enm)
	(define damaged-recently (enm-damaged-recently enm))
	(when (fxpositive? damaged-recently)
	  (enm-damaged-recently-set! enm (fx1- damaged-recently)))
	(enm-ox-set! enm (enm-x enm))
	(enm-oy-set! enm (enm-y enm))
	(when (is-boss? enm)
	  (let* ([bossinfo (enm-extras enm)]
			 [old-xs (bossinfo-old-xs bossinfo)]
			 [old-ys (bossinfo-old-ys bossinfo)])
		;; shift left by one
		(flvector-copy! old-xs 1
						old-xs 0
						(fx1- +boss-lazy-spellcircle-context+))
		(flvector-copy! old-ys 1
						old-ys 0
						(fx1- +boss-lazy-spellcircle-context+))
		;; add the new positions at end
		(flvector-set! old-xs (fx1- +boss-lazy-spellcircle-context+) (enm-x enm))
		(flvector-set! old-ys (fx1- +boss-lazy-spellcircle-context+) (enm-y enm)))))
  (vector-for-each-truthy each live-enm))

(define (posttick-enemies)
  (define (each enm)
	(when (is-boss? enm)
	  (let* ([bossinfo (enm-extras enm)]
			 [timer (bossinfo-remaining-timer bossinfo)])
		(unless (fxzero? timer)
		  (bossinfo-remaining-timer-set! bossinfo (fx1- timer))
		  (when (and (fx<= timer 600)
					 (fxzero? (fxmod timer 60)))
			(if (fx<= timer 300)
			  (raylib:play-sound (sebundle-timeoutwarn sounds))
			  (raylib:play-sound (sebundle-timeout sounds)))))))
	(when (positive? (enm-superarmor enm))
	  (enm-superarmor-set! enm (sub1 (enm-superarmor enm))))
	(let* ([ox (enm-ox enm)] [oy (enm-oy enm)]
		   [x (enm-x enm)] [y (enm-y enm)]
		   [stationary-x (epsilon-equal ox x)]
		   [stationary-y (epsilon-equal oy y)]
		   [dx (enm-dx-render enm)])
	  (cond
	   ;; Didn't move: don't update at all
	   [(and stationary-x stationary-y) #f]
	   ;; Stationary on x (moving on y): Go back towards zero
	   ;; TODO: Do we really want this?
	   [stationary-x
		(cond
		 [(flnegative? dx) (enm-dx-render-set!
							enm (flmin 0.0 (fl+ dx (abs (fl- y oy)))))]
		 [(flpositive? dx) (enm-dx-render-set!
							enm (flmax 0.0 (fl- dx (abs (fl- y oy)))))])]
	   ;; Moving on x: Go in the direction we're moving (up to a cap)
	   [else
		(enm-dx-render-set! enm (clamp (fl+ dx (fl- x ox)) -10.0 10.0))])))
  (vector-for-each-truthy each live-enm))

;; TODO put the optional args in an alist or something?
(define spawn-enemy
  (case-lambda
	[(type x y health control-function)
	 (spawn-enemy type x y health control-function default-drop)]
	[(type x y health control-function drops)
	 (spawn-enemy type x y health control-function drops #f)]
	[(type x y health control-function drops on-death)
	 (let ((idx (vector-index #f live-enm)))
	   (unless idx
		 (error 'spawn-enemy "No more open enemy slots!"))
	   (let ([enemy (make-enm type x y x y health 0 empty-enmflags
							  drops on-death 0 0.0 #f)])
		 (vector-set! live-enm idx enemy)
		 (spawn-task
		  (symbol->string type)
		  (lambda (task) (control-function task enemy))
		  (thunk (eq? enemy (vnth live-enm idx))))
		 enemy))]))

(define (delete-enemy enm)
  (let ([idx (vector-index enm live-enm)])
	(when idx
	  (vector-set! live-enm idx #f))))

(define (kill-enemy enm)
  (let ([do-standard-logic (or (not (enm-on-death enm))
							   ((enm-on-death enm)))])
	(when do-standard-logic
	  (spawn-enm-drops enm)
	  (raylib:play-sound (sebundle-enmdie sounds))
	  (spawn-particle (make-particle
					   (particletype enmdeath)
					   (+ (enm-x enm) (centered-roll visual-rng 20.0))
					   (+ (enm-y enm) (centered-roll visual-rng 20.0))
					   30 0 '((start-radius . 2)
							  (end-radius . 85))))
	  (let ([idx (vector-index enm live-enm)])
		(when idx
		  (vector-set! live-enm idx #f))))))

(define damage-enemy
  (case-lambda
	[(enm amount)
	 (damage-enemy enm amount #t)]
	[(enm amount playsound)
	 (set! current-score (+ current-score 20))
	 (if (enm-hasflag? enm (enmflag invincible))
		 (when playsound
		   (raylib:play-sound (sebundle-damageresist sounds)))
		 (let* ([superarmor (positive? (enm-superarmor enm))]
				[amount (cond
						 [superarmor
						  (max 1 (eround (* 0.1 amount)))]
						 [else amount])])
		   (when (and playsound (> amount 0))
			 (raylib:play-sound (if superarmor
									(sebundle-damageresist sounds)
									(sebundle-damage0 sounds))))
		   (when (> amount 0)
			 (enm-damaged-recently-set! enm 20)
			 (enm-health-set! enm (- (enm-health enm) amount)))

		   (when (fx<= (enm-health enm) 0)
			 (kill-enemy enm))))]))

(define (spawn-drops drops x y)
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
			  [fuzz-x (if skip-fuzz 0.0 (- (roll game-rng 100) 50.0))]
			  [fuzz-y (if skip-fuzz 0.0 (- (roll game-rng 30) 15.0))])
		 (let ([ent (spawn-misc-ent
					 type
					 (+ x fuzz-x) (+ y fuzz-y)
					 -3.0 0.08)])
		   (when (fxpositive? bombing)
			 (spawn-task "delayed autocollect"
						 (lambda (task) (wait 45) (miscent-autocollect-set! ent #t))
						 (constantly #t)))))))
   drops))

(define (spawn-enm-drops enm)
  (spawn-drops (enm-drops enm) (enm-x enm) (enm-y enm)))

(define (damage-player)
  ;; if player is already dying, don't reset this
  (when (and (not (player-invincible?))
			 (fxzero? death-timer))
	(raylib:play-sound (sebundle-playerdie sounds))
	(set! death-timer +deathbomb-time+)))

(define (kill-player)
  (set! iframes 180)
  (fail-current-attack)
  (unless force-invincible
	(raylib:play-sound (sebundle-shoot0 sounds))
	(when (>= life-stock 1)
	  (set! life-stock (sub1 life-stock))) ;; todo gameovering
	(when (< bomb-stock 3)
	  (set! bomb-stock 3))

	(set! respawning +respawning-max+)
	(set! player-x 0.0)
	(set! player-y +initial-player-y+)))

(define (check-laser-collision lx ly rotation lradius length
							   px py pradius)
  ;; the strategy is to invert reference frame so that the laser is at
  ;; position 0, 0 with rotation 0
  ;; Then we just use axis-aligned collision testing
  (let* ([neg-rotation (fl- rotation)]
		 [cos-neg-theta (flcos neg-rotation)]
		 [sin-neg-theta (flsin neg-rotation)]
		 [pxt (fl- px lx)]
		 [pyt (fl- py ly)]
		 [pxr (fl- (fl* pxt cos-neg-theta) (fl* pyt sin-neg-theta))]
		 [pyr (fl+ (fl* pxt sin-neg-theta) (fl* pyt cos-neg-theta))])
	;; now in this reference frame, the laser point is at 0, 0.
	;; its hitbox extends `lradius` above and below and `length` to the right
	(check-collision-circle-rec
	 pxr pyr pradius
	 0.0 (fl- lradius) length (fl* 2.0 lradius))))

(define (check-player-collision bullet player-radius)
  (define is-laser (eq? 'fixed-laser (bullet-family (bullet-type bullet))))
  (if is-laser
	  (check-laser-collision (bullet-x bullet) (bullet-y bullet)
							 (bullet-facing bullet)
							 (laser-radius bullet)
							 (laser-length bullet)
							 player-x player-y player-radius)
	  (check-collision-circles
	   player-x player-y player-radius
	   (bullet-x bullet) (bullet-y bullet)
	   (bullet-hit-radius (bullet-type bullet)))))

(define (process-collisions)
  (define (each-bullet bullet)
	(define is-laser (eq? 'fixed-laser (bullet-family (bullet-type bullet))))
	(when (bullet-active? bullet)
	  (when (and (if is-laser
					 (fx>= (fx- frames (laser-last-grazed-at bullet)) 4)
					 (not (bullet-grazed bullet)))
				 (check-player-collision bullet graze-radius))
		(set! graze (fx1+ graze))
		(set! item-value (fx+ 10 item-value))
		(set! current-score (fx+ 1000 current-score))
		(if is-laser
			(laser-last-grazed-at-set! bullet frames)
			(bullet-grazed-set! bullet #t))
		(spawn-particle (make-particle
						 (particletype graze)
						 player-x player-y
						 25 0
						 (list (cons 'dir
									 ;; 80% of the time, particle flies towards bullet
									 ;; 20% of the time, away from it
									 (torad (+ (centered-roll visual-rng 90.0)
											   (if (< (bullet-x bullet) player-x)
												   180.0 0.0)
											   (if (< (roll visual-rng) 0.2)
												   180.0 0.0))))
							   (cons 'speed (+ 1.0 (* 2.0 (roll visual-rng))))
							   (cons 'rot (* (roll visual-rng) 360.0)))))
		(raylib:play-sound (sebundle-graze sounds)))

	  (when (check-player-collision bullet hit-radius)
		(unless is-laser
		  (cancel-bullet bullet))
		(damage-player))))
  (define (each-enm enm)
	(unless (enm-hasflag? enm (enmflag nocollide))
	  (let-values ([(x y w h) (enm-collision-box enm)])
		(when (check-collision-circle-rec
			   player-x player-y hit-radius
			   x y w h)
		  (damage-player)))))
  (vector-for-each-truthy each-bullet live-bullets)
  (vector-for-each-truthy each-enm live-enm))

(define (enm-collision-box enm)
  (case (enm-type enm)
	([red-fairy green-fairy blue-fairy yellow-fairy]
	 (values (- (enm-x enm) 8)
			 (- (enm-y enm) 8)
			 16 16))
	([boss-doremi boss-hazuki boss-aiko boss-onpu]
	 (values (- (enm-x enm) 24)
			 (- (enm-y enm) 24)
			 48 48))
	([medium-blue-fairy medium-red-fairy
						red-yinyang green-yinyang blue-yinyang magenta-yinyang]
	 (values (- (enm-x enm) 11)
			 (- (enm-y enm) 11)
			 22 22))
	([big-fairy]
	 (values (- (enm-x enm) 15)
			 (- (enm-y enm) 15)
			 30 30))))

(define (enm-hurtbox enm)
  (case (enm-type enm)
	([red-fairy green-fairy blue-fairy yellow-fairy]
	 (values (- (enm-x enm) 16)
			 (- (enm-y enm) 16)
			 32 32))
	([boss-doremi boss-hazuki boss-aiko boss-onpu]
	 (values (- (enm-x enm) 24)
			 (- (enm-y enm) 24)
			 48 48))
	([medium-blue-fairy medium-red-fairy
						red-yinyang green-yinyang blue-yinyang magenta-yinyang]
	 (values (- (enm-x enm) 16)
			 (- (enm-y enm) 16)
			 32 32))
	([big-fairy]
	 (values (- (enm-x enm) 22)
			 (- (enm-y enm) 22)
			 44 44))))

(define (draw-boss textures enm render-x render-y)
  (define bossinfo (enm-extras enm))
  (define lazy-render-x (+ +playfield-render-offset-x+
						   (flvector-ref (bossinfo-old-xs bossinfo) 0)))
  (define lazy-render-y (+ +playfield-render-offset-y+
						   (flvector-ref (bossinfo-old-ys bossinfo) 0)))
  (when (bossinfo-aura-active bossinfo)
	(let ([radius (fl+ 90.0 (fl* 10.0 (flsin (/ frames 12.0))))])
	  (draw-sprite-pro-with-rotation
	   textures 'magicircle
	   (flmod (* frames 3.0) 360.0)
	   ;; idk why I don't subtract the radius here but it works so :shrug:
	   (make-rectangle render-x render-y
					   (fl* 2.0 radius) (fl* 2.0 radius))
	   #xffffffff)))
	
  ;; Okay, so raylib's draw-ring uses the ENTIRE shapes-texture for
  ;; every segment of the circle which is...not what we want.
  ;; However, I tried implementing ring drawing myself in the previous commit
  ;; and for some reason transparency is not working for it even though
  ;; my code looks exactly like what Raylib would do...
  ;; The compromise is to use Raylib's DrawRing, but only for a small sector (i.e. segments=1).
  ;; while having a loop on the Scheme side to continuously update the shape rect
  ;; TODO: Find a permanent solution for this clowntown
  (when (bossinfo-active-spell-name bossinfo)
	(let* ([boss-tex (txbundle-boss-flip textures)]
		   [save-tex (raylib:get-shapes-texture)]
		   [save-rect (raylib:get-shapes-texture-rectangle)]
		   [elapsed-frames (fx- (bossinfo-total-timer bossinfo)
								(bossinfo-remaining-timer bossinfo))])
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
							 (bitwise-ior #xffffff00 inner-ring-brightness)))))

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
  (draw-sprite textures 'yellow-fairy2 render-x render-y -1))

(define (draw-enemies textures)
  (define (each enm)
	(let ([render-x (+ (enm-x enm) +playfield-render-offset-x+)]
		  [render-y (+ (enm-y enm) +playfield-render-offset-y+)]
		  [dx (enm-dx-render enm)]
		  [type (enm-type enm)]
		  [tint (cond
				 [(and (enm-hasflag? enm (enmflag invincible))
					   (< (fxmod frames 4) 2))
				  invincible-flash]
				 [(and (fxpositive? (enm-damaged-recently enm))
					   (< (fxmod frames 4) 2))
				  damage-flash]
				 [else -1])])
	  (case type
		([boss-doremi boss-hazuki boss-aiko boss-onpu]
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
			(flmod (* frames -4.0) 360.0)
			1.4 render-x render-y tint)
		   (draw-sprite-with-rotation
			textures
			outer-sprite
			(flmod (* frames 8.0) 360.0)
			render-x render-y tint)))
		([yellow-fairy red-fairy green-fairy blue-fairy
					   medium-red-fairy medium-blue-fairy big-fairy]
		 (cond
		  [(fl< (abs dx) 5.0)
		   (let* ([fwd-sprites
				   (case type
					 ([yellow-fairy] '#(yellow-fairy1 yellow-fairy2
													  yellow-fairy3 yellow-fairy4))
					 ([red-fairy] '#(red-fairy1 red-fairy2 red-fairy3 red-fairy4))
					 ([green-fairy] '#(green-fairy1 green-fairy2 green-fairy3 green-fairy4))
					 ([blue-fairy] '#(blue-fairy1 blue-fairy2 blue-fairy3 blue-fairy4))
					 ([medium-blue-fairy] '#(medium-blue-fairy0 medium-blue-fairy1 medium-blue-fairy2 medium-blue-fairy3))
					 ([medium-red-fairy] '#(medium-red-fairy0 medium-red-fairy1 medium-red-fairy2 medium-red-fairy3))
					 ([big-fairy] '#(big-fairy0 big-fairy1 big-fairy2 big-fairy3)))]
				  [sprite (vnth fwd-sprites
								(truncate (mod (/ frames 5)
											   (vlen fwd-sprites))))])
			 (draw-sprite textures sprite render-x render-y tint))]
		  [(fl< (abs dx) 10.0)
		   (let* ([transition-sprites
				   (case type
					 ([yellow-fairy] '#(yellow-fairy5))
					 ([red-fairy] '#(red-fairy5))
					 ([green-fairy] '#(green-fairy5))
					 ([blue-fairy] '#(blue-fairy5))
					 ([medium-blue-fairy] '#(medium-blue-fairy4 medium-blue-fairy5 medium-blue-fairy6 medium-blue-fairy7))
					 ([medium-red-fairy] '#(medium-red-fairy4 medium-red-fairy5 medium-red-fairy6 medium-red-fairy7))
					 ([big-fairy] '#(big-fairy4 big-fairy5 big-fairy6 big-fairy7)))]
				  [sprite (vnth transition-sprites
								(truncate (mod (/ frames 7)
											   (vlen
												transition-sprites))))])
			 (if (flnegative? dx)
				 (draw-sprite-mirror-x textures sprite render-x render-y tint)
				 (draw-sprite textures sprite render-x render-y tint)))]
		  [else
		   (let* ([side-sprites
				   (case type
					 ([yellow-fairy]
					  '#(yellow-fairy6 yellow-fairy7 yellow-fairy8
									   yellow-fairy9 yellow-fairy10 yellow-fairy11))
					 ([red-fairy]
					  '#(red-fairy6 red-fairy7 red-fairy8
									red-fairy9 red-fairy10 red-fairy11))
					 ([green-fairy]
					  '#(green-fairy6 green-fairy7 green-fairy8
									  green-fairy9 green-fairy10 green-fairy11))
					 ([blue-fairy]
					  '#(blue-fairy6 blue-fairy7 blue-fairy8
									 blue-fairy9 blue-fairy10 blue-fairy11))
					 ([medium-blue-fairy] '#(medium-blue-fairy8 medium-blue-fairy9 medium-blue-fairy10 medium-blue-fairy11))
					 ([medium-red-fairy] '#(medium-red-fairy8 medium-red-fairy9 medium-red-fairy10 medium-red-fairy11))
					 ([big-fairy] '#(big-fairy8 big-fairy9 big-fairy10 big-fairy11)))]
				  [sprite (vnth side-sprites
								(truncate (mod (/ frames 7)
											   (vlen side-sprites))))])
			 (if (flnegative? dx)
				 (draw-sprite-mirror-x textures sprite render-x render-y tint)
				 (draw-sprite textures sprite render-x render-y tint)))])))
	  (when show-hitboxes
		(let-values ([(x y w h) (enm-hurtbox enm)])
		  (raylib:draw-rectangle-rec
		   (+ x +playfield-render-offset-x+)
		   (+ y +playfield-render-offset-y+) w h green))
		(unless (enm-hasflag? enm (enmflag nocollide))
		  (let-values ([(x y w h) (enm-collision-box enm)])
			(raylib:draw-rectangle-rec
			 (+ x +playfield-render-offset-x+)
			 (+ y +playfield-render-offset-y+) w h red)))
		)))
  (vector-for-each-truthy each live-enm))

(define (linear-step-forever facing speed blt)
  (bullet-facing-set! blt facing)
  (loop-forever (linear-step facing speed blt)))

(define (linear-step facing speed blt)
  (bullet-x-set! blt (+ (bullet-x blt) (* speed (cos facing))))
  (bullet-y-set! blt (+ (bullet-y blt) (* speed (sin facing)))))

(define (linear-step-enm-forever facing speed enm)
  (loop-forever (linear-step-enm facing speed enm)))

(define (linear-step-enm facing speed enm)
  (enm-x-set! enm (+ (enm-x enm) (* speed (cos facing))))
  (enm-y-set! enm (+ (enm-y enm) (* speed (sin facing)))))

(define (linear-step-separate vx vy blt)
  (define ox (bullet-x blt))
  (define oy (bullet-y blt))
  (define nx (fl+ ox vx))
  (define ny (fl+ oy vy))
  (bullet-x-set! blt nx)
  (bullet-y-set! blt ny)
  (bullet-facing-set! blt (flatan (fl- ny oy) (fl- nx ox))))
(define linear-step-gravity-forever
  (case-lambda
	([facing speed ay blt]
	 (linear-step-gravity-forever facing speed ay +inf.0 blt))
	([facing speed ay max-vy blt]
	 (define vx (* speed (cos facing)))
	 (define vy (* speed (sin facing)))
	 (bullet-facing-set! blt facing)
	 (let loop ([vy vy])
	   (linear-step-separate vx vy blt)
	   (yield)
	   (loop (min (+ vy ay) max-vy))))))

;; Returns once the bullet reaches stationary speed
;; acceleration should be negative
(define (linear-step-decelerate facing speed accel blt)
  (bullet-facing-set! blt facing)
  (let loop ([v speed])
	(linear-step-separate (fl* v (flcos facing))
						  (fl* v (flsin facing))
						  blt)
	(yield)
	(let ([next-v (fl+ v accel)])
	  (when (flpositive? next-v)
		(loop next-v)))))

;; Returns once the bullet reaches max-speed
(define (linear-step-accelerate facing speed accel max-speed blt)
  (bullet-facing-set! blt facing)
  (let loop ([v speed])
	(linear-step-separate (fl* v (flcos facing))
						  (fl* v (flsin facing))
						  blt)
	(yield)
	(let ([next-v (fl+ v accel)])
	  (when (fl< next-v max-speed)
		(loop next-v)))))
;; Maintains max-speed once it's reached
(define (linear-step-accelerate-forever facing speed accel max-speed blt)
  (linear-step-accelerate facing speed accel max-speed blt)
  (linear-step-forever facing max-speed blt))

(define-record-type particle
  (fields
   type
   (mutable x)
   (mutable y)
   max-age
   (mutable age)
   extra-data))

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
	(case(particle-type p)
	  ([graze maple maple-grayscale]
	   (let ([dir (cdr (assq 'dir (particle-extra-data p)))]
			 [speed (cdr (assq 'speed (particle-extra-data p)))])
		 (particle-x-set! p (+ (particle-x p) (* speed (cos dir))))
		 (particle-y-set! p (+ (particle-y p) (* speed (sin dir)))))))
	(when (fx> (particle-age p) (particle-max-age p))
	  (delete-particle p)))
  (vector-for-each-truthy each live-particles))

(define (draw-particles textures fonts)
  (define (each p)
	(define render-x (+ (particle-x p) +playfield-render-offset-x+))
	(define render-y (+ (particle-y p) +playfield-render-offset-y+))
	(define extra-data (particle-extra-data p))
	(case (particle-type p)
	  ([maple maple-grayscale]
	   (let* ([t (/ (particle-age p) (particle-max-age p))]
			  [sz (lerp (cdr (assq 'initsz extra-data)) 16 t)]
			  [rot (cdr (assq 'rot extra-data))])
		 (draw-sprite-pro-with-rotation
		  textures (particle-type p)
		  (fl+ rot (flmod (* frames 3.0) 360.0))
		  (make-rectangle render-x render-y sz sz)
		  (packcolor 255 255 255 (eround (lerp 255 0 t))))))
	  ([graze]
	   (let* ([t (/ (particle-age p) (particle-max-age p))]
			  [sz (lerp 6 0 t)]
			  [rot (cdr (assq 'rot (particle-extra-data p)))])
		 (raylib:draw-rectangle-pro render-x render-y sz sz
									(/ sz 2) (/ sz 2)
									rot #xf5f5f5f5)))
	  ([enmdeath]
	   (let ([age (/ (particle-age p) (particle-max-age p))])
		 (raylib:draw-circle-lines-v
		  render-x render-y
		  (inexact (lerp (cdr (assq 'start-radius extra-data))
						 (cdr (assq 'end-radius extra-data))
						 age))
		  (packcolor 255 255 255 (eround (lerp 255 0 age))))))
	  ([cancel]
	   (let* ([age (floor (/ (particle-age p) 3))]
			  [v (if (fx< age 4) 0.0 64.0)]
			  [u (* 64 (fxmod age 4))])
		 (raylib:draw-texture-pro
		  (txbundle-bulletcancel textures)
		  (make-rectangle u v 64.0 64.0)
		  (make-rectangle (- render-x 24.0) (- render-y 24.0) 48.0 48.0)
		  v2zero 0.0 -1)))
	  ([itemvalue]
	   (let ([alpha (round (lerp 255 0
								 (ease-in-quad
								  (/ (particle-age p) (particle-max-age p)))))]
			 [color (car extra-data)]
			 [value (cdr extra-data)]
			 [render-x (fl+ render-x 10.0)])
		 (let-values ([(width _) (raylib:measure-text-ex
								  (fontbundle-cabin fonts)
								  value 16.0 0.0)])
		   (raylib:draw-text-ex
			(fontbundle-cabin fonts)
			value
			;; display to the left if we wouldn't have room on the right
			;; this is done here instead of when the particle is spawned so that
			;; we don't have to pass the fonts to the game logic
			(if (> (+ render-x width) +playfield-max-render-x+)
				(eround (- render-x width 20.0))
				(eround render-x))
			(eround render-y)
			16.0 0.0 (bitwise-ior color alpha)))))
	  ([circle-hint]
	   (let ([color (cdr (assq 'color extra-data))]
			 [r1 (cdr (assq 'r1 extra-data))]
			 [r2 (cdr (assq 'r2 extra-data))])
		 (raylib:draw-circle-lines-v
		  render-x render-y
		  (lerp r1 r2 (/ (particle-age p) (particle-max-age p)))
		  color)))
	  ([spellbonus]	   
	   (let*-values ([(width _) (raylib:measure-text-ex
								 (fontbundle-cabin fonts)
								 extra-data 24.0 0.0)])
		 (raylib:draw-text-ex
		  (fontbundle-cabin fonts)
		  extra-data
		  (+ +playfield-render-offset-x+ (/ width -2.0))
		  75.0
		  24.0 0.0
		  (if (< (particle-age p) 120)
			  -1
			  (packcolor
			   255 255 255
			   (eround (lerp 255 0
							 (/ (- (particle-age p) 120)
								(- (particle-max-age p) 120)))))))))))
  (vector-for-each-truthy each live-particles))

(define-record-type miscent
  (fields
   type
   (mutable x)
   (mutable y)
   (mutable vy)
   (mutable ay)
   (mutable livetime)
   (mutable autocollect)))

(define (miscent-supports-autocollect? ent)
  (not (eq? (miscent-type ent) 'mainshot)))

(define (autocollect-all-items)
  (vector-for-each-truthy
   (lambda (ent)
	 (when (miscent-supports-autocollect? ent)
	   (miscent-autocollect-set! ent #t)))
   live-misc-ents))

(define live-misc-ents
  (make-vector 4096 #f))

(define (spawn-misc-ent type x y vy ay)
  (let ((idx (vector-index #f live-misc-ents)))
	(unless idx
	  (error 'spawn-misc-ent "No more open misc entity slots!"))
	(let ([ent (make-miscent type x y vy ay 0 #f)])
	  (vector-set! live-misc-ents idx ent)
	  ent)))

(define (delete-misc-ent ent)
  (let ((idx (vector-index ent live-misc-ents)))
	(when idx
	  ;; tolerate killing already-removed/dead
	  (vector-set! live-misc-ents idx #f))))

(define (add-bombs amt)
  (define old-stock bomb-stock)
  (define limit 8)
  (define new-stock (+ bomb-stock amt))
  (define amt-over-limit (- new-stock limit))
  (if (positive? amt-over-limit)
	  (let ()
		(set! bomb-stock limit)
		;; nb: must be divisible by 3
		(set! item-value (+ item-value (* 6000 amt-over-limit))))
	  (set! bomb-stock new-stock))
  (if (not (= (floor old-stock) (floor bomb-stock)))
	  (raylib:play-sound (sebundle-spellcapture sounds))
	  (raylib:play-sound (sebundle-item sounds))))

(define (add-lives amt)
  (define old-stock life-stock)
  (define limit 8)
  (define new-stock (+ life-stock amt))
  (define amt-over-limit (- new-stock limit))
  (if (positive? amt-over-limit)
	  (let ()
		(set! life-stock limit)
		(add-bombs amt-over-limit))
	  (set! life-stock new-stock))
  (if (not (= (floor old-stock) (floor life-stock)))
	  (raylib:play-sound (sebundle-extend sounds))
	  (raylib:play-sound (sebundle-item sounds))))

(define (tick-misc-ents)
  (define (each ent)
	(define type (miscent-type ent))
	(define terminal-velocity
	  (case type
		([point life-frag big-piv life bomb-frag small-piv bomb] 10.0)
		(else +inf.0)))
	(define (do-standard-movement)
	  (let ([vy (miscent-vy ent)]
			[ay (miscent-ay ent)])
		(when (not (zero? vy))
	  	  (miscent-y-set! ent (+ (miscent-y ent) vy)))
		(when (and (not (zero? ay)) (< vy terminal-velocity))
		  (miscent-vy-set! ent (+ vy ay)))))
	(case type
	  ((mainshot)
	   (do-standard-movement)
	   (call/1cc
		(lambda (return)
		  (vector-for-each-truthy
		   (lambda (enm)
			 (let-values ([(ehx ehy ehw ehh) (enm-hurtbox enm)])
			   (when (and
					  (not (enm-hasflag? enm (enmflag invincible)))
					  (check-collision-recs
					   ehx ehy ehw ehh
					   (- (miscent-x ent) 6)
					   (- (miscent-y ent) 10)
					   12 16))
				 (delete-misc-ent ent)
				 (damage-enemy enm 25)
				 (return))))
		   live-enm)
		  (when (< (miscent-y ent) +playfield-min-y+)
			(delete-misc-ent ent)))))
	  ([needle]
	   (do-standard-movement)
	   (call/1cc
		(lambda (return)
		  (vector-for-each-truthy
		   (lambda (enm)
			 (let-values ([(ehx ehy ehw ehh) (enm-hurtbox enm)])
			   (when (and
					  (not (enm-hasflag? enm (enmflag invincible)))
					  (check-collision-recs
					   ehx ehy ehw ehh
					   (- (miscent-x ent) 5)
					   (- (miscent-y ent) 6)
					   10 52))
				 (delete-misc-ent ent)
				 (damage-enemy enm 10)
				 (return))))
		   live-enm)
		  (when (< (miscent-y ent) (- +playfield-min-y+ 50)) ;; extra fuzz for length
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
		[(check-collision-circle-rec
		  player-x player-y (if focused-immediate vacuum-radius-focused
								vacuum-radius-unfocused)
		  (- (miscent-x ent) 8) (- (miscent-y ent) 8)
		  16 16)
		 (let ([dir-to-player (v2unit (vec2 (- player-x (miscent-x ent))
											(- player-y (miscent-y ent))))])
		   (miscent-x-set! ent (+ (miscent-x ent) (* (v2x dir-to-player) 6)))
		   (miscent-y-set! ent (+ (miscent-y ent) (* (v2y dir-to-player) 6)))
		   ;; reset this to something reasonable
		   (miscent-vy-set! ent 1.0))]
		[else (do-standard-movement)])
	   (when (check-collision-circle-rec
			  player-x player-y hit-radius
			  (- (miscent-x ent) 8) (- (miscent-y ent) 8)
			  16 16)
		 (case type
		   ([point]
			(raylib:play-sound (sebundle-item sounds))
			(let* ([value-multiplier (if (or (miscent-autocollect ent) (< player-y +poc-y+))
										 1.0
										 (max 0.25
											  (- 1.0 (/ (- player-y +poc-y+)
														(- +playfield-max-y+ +poc-y+)))))]
				   [value (round-score (* item-value value-multiplier))])
			  (set! current-score (+ current-score value))
			  (spawn-particle
			   (make-particle (particletype itemvalue)
							  (miscent-x ent) (- (miscent-y ent) 10.0)
							  60 0 (cons (if (= 1 value-multiplier)
											 #xffd70000
											 #xf5f5f500)
										 (number->string value))))))
		   ([life-frag] (add-lives 1/3))
		   ([life] (add-lives 1))
		   ([bomb-frag] (add-bombs 1/3))
		   ([bomb] (add-bombs 1))
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
	(miscent-livetime-set! ent (add1 (miscent-livetime ent))))
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
		render-x render-y #xffffffdd)
	   (when show-hitboxes
		 (raylib:draw-rectangle-rec
		  (- render-x 6) (- render-y 10) 12 16
		  red)
		 (raylib:draw-circle-v render-x render-y 2.0 green)))
	  ([needle]
	   ;; todo: different rendering when it hits something?
	   (draw-sprite-with-rotation
		textures 'needle -90
		render-x render-y #xffffffdd)
	   (when show-hitboxes
		 (raylib:draw-rectangle-rec
		  (- render-x 5) (- render-y 6) 10 52
		  red)
		 (raylib:draw-circle-v render-x render-y 2.0 green)))
	  ([point life-frag big-piv life bomb-frag small-piv bomb]
	   (let ([livetime (miscent-livetime ent)])
		 (if (and (fx<= livetime 24) (miscent-should-spin? ent))
			 (draw-sprite-with-rotation
			  textures type (* 45.0 (floor (/ livetime 3)))
			  render-x render-y -1)
			 (draw-sprite textures type render-x render-y
						  (if (member type '(big-piv small-piv)) #xffffffc0 -1))))
	   (when show-hitboxes
		 (raylib:draw-rectangle-rec
		  (- render-x 8) (- render-y 8) 16 16
		  red)))))
  (vector-for-each-truthy each live-misc-ents))

(define (ease-to easer x y duration enm)
  (define x0 (enm-x enm))
  (define y0 (enm-y enm))
  (do ([i 0 (fx1+ i)])
	  ((fx> i duration))
	(let* ([progress (/ i duration)]
		   [eased (easer (clamp progress 0.0 1.0))]
		   [x (+ x0 (* eased (- x x0)))]
		   [y (+ y0 (* eased (- y y0)))])
	  (enm-x-set! enm x)
	  (enm-y-set! enm y))
	(yield)))
(define (ease-linear-to x y duration enm)
  (ease-to values x y duration enm))
(define (ease-cubic-to x y duration enm)
  (ease-to ease-out-cubic x y duration enm))

;; points must be a whole spline (see truncate-to-whole-spline)
(define (move-on-spline points segment->config enm)
  (do [(segment 0 (add1 segment))
	   (i 0 (+ i 3))]
	  [(= i (sub1 (vlen points)))]
	(let-values ([(easer duration) (segment->config segment)]
				 [(p0) (vnth points i)]
				 [(p1) (vnth points (+ 1 i))]
				 [(p2) (vnth points (+ 2 i))]
				 [(p3) (vnth points (+ 3 i))])
	  ;; NB: I know that movement along a curve actually isn't trivial,
	  ;; and doing steps like this isn't going to give me exact results,
	  ;; but it works well enough for what we're doing in this game
	  (do [(j 0 (fx1+ j))]
		  [(fx> j duration)]
		(let* ([progress (/ j duration)]
			   [eased (easer (clamp progress 0.0 1.0))]
			   [p (eval-bezier-cubic p0 p1 p2 p3 eased)])
		  (enm-x-set! enm (v2x p))
		  (enm-y-set! enm (v2y p)))
		(yield)))))

(define (declare-spell boss spell-name duration-frames health bonus)
  (define bossinfo (enm-extras boss))
  (bossinfo-active-spell-name-set! bossinfo spell-name)
  (bossinfo-active-spell-bonus-set! bossinfo bonus)
  (bossinfo-remaining-timer-set! bossinfo duration-frames)
  (bossinfo-total-timer-set! bossinfo duration-frames)
  (bossinfo-max-health-set! bossinfo health)
  (enm-superarmor-set! boss 120)
  (enm-health-set! boss health)
  (raylib:play-sound (sebundle-spelldeclare sounds))
  (bossinfo-active-attack-failed-set! bossinfo #f))

(define (declare-nonspell boss duration-frames health)
  (define bossinfo (enm-extras boss))
  (bossinfo-remaining-timer-set! bossinfo duration-frames)
  (bossinfo-total-timer-set! bossinfo duration-frames)
  (bossinfo-max-health-set! bossinfo health)
  (enm-health-set! boss health))

(define (test-fairy-control2-ring1 enm)
  (define b (-> (cb)
				(cbcount 50)
				(cbspeed 1.0)
				(cbang 40.0 0.0)))
  (interval-loop
   90
   (raylib:play-sound (sebundle-shoot0 sounds))
   (cbshoot b (enm-x enm) (enm-y enm)
			(lambda (row col speed facing)
			  (spawn-bullet
			   'arrow-green (enm-x enm) (enm-y enm)
			   5
			   (curry linear-step-forever facing speed))))))

(define (test-fairy-control2-ring2 enm)
  (define b (-> (cb)
				(cbcount 5)
				(cbspeed 5.0)))
  (interval-loop
   30
   (raylib:play-sound (sebundle-bell sounds))
   (cbshoot b (enm-x enm) (enm-y enm)
			(lambda (row col speed facing)
			  (spawn-bullet
			   'small-ball-blue (enm-x enm) (enm-y enm)
			   10
			   (curry linear-step-forever facing speed))))))

(define (common-spell-postlude bossinfo enm)
  (define failed (or (bossinfo-active-attack-failed bossinfo)
					 ;; ran out of time and not a survival
					 (and (not (= -1 (bossinfo-max-health bossinfo)))
						  (positive? (enm-health enm)))))
  (define bonus (and (not failed) (calculate-spell-bonus bossinfo)))
  (spawn-enm-drops enm)
  (raylib:play-sound (sebundle-shoot0 sounds))
  (unless failed
	(raylib:play-sound (sebundle-spellcapture sounds))
	(set! current-score (+ current-score bonus)))
  (spawn-particle
   (make-particle
	(particletype spellbonus)
	;; Position dynamically calculated at render to avoid
	;; the enemy tasks needing to access the fonts
	0.0 0.0 180 0
	(if failed
		"Bonus Failed..."
		(format "GET Spell Bonus!! ~:d" bonus))))
  (cancel-all #t)
  (bossinfo-active-spell-name-set! bossinfo #f)
  (bossinfo-active-spell-bonus-set! bossinfo #f))

(define (common-boss-postlude bossinfo enm)
  (raylib:play-sound (sebundle-bossdie sounds))
  (spawn-task "particles"
			  (lambda (task)
				(dotimes 90
				  (spawn-particle
				   (make-particle
					(particletype maple)
					(fl+ (enm-x enm) (centered-roll visual-rng 5.0))
					(fl+ (enm-y enm) (centered-roll visual-rng 5.0))
					60 0 `((speed . ,(fl+ (centered-roll visual-rng 0.75) 1.5))
						   (dir . ,(centered-roll visual-rng pi))
						   (rot . ,(fl* (roll visual-rng) 360.0))
						   (initsz . 55))))
				  (yield)))
			  (constantly #t))
  (ease-to values
		   (fl+ (enm-x enm) (centered-roll game-rng 40.0))
		   (fl+ (enm-y enm) (centered-roll game-rng 30.0))
		   90 enm)
  (raylib:play-sound (sebundle-bossdie sounds))
  (cancel-all #t)
  (dotimes 90
	(spawn-particle
	 (make-particle (particletype maple)
					(fl+ (enm-x enm) (centered-roll visual-rng 2.0))
					(fl+ (enm-y enm) (centered-roll visual-rng 2.0))
					60 0 `((speed . ,(fl+ (centered-roll visual-rng 1.5) 2.5))
						   (dir . ,(centered-roll visual-rng pi))
						   (rot . ,(fl* (roll visual-rng) 360.0))
						   (initsz . 70)))))
  (delete-enemy enm))

(define (test-fairy-control2 task enm)
  (test-fairy-non1 task enm)
  (delete-enemy enm))

(define (test-fairy-non1 task enm)
  (define bossinfo (enm-extras enm))
  (define keep-running
	(lambda () (and (positive? (enm-health enm))
					(positive? (bossinfo-remaining-timer bossinfo)))))
  (bossinfo-dummy-healthbars-set! bossinfo
								  (vector (make-dummy-healthbar
										   10 0
										   #xf50000ff
										   #x800000ff)))
  (declare-nonspell enm 1200 1000)
  (spawn-subtask
	  "non1-ring1"
	(lambda (_) (test-fairy-control2-ring1 enm))
	keep-running task)
  (wait-while keep-running)
  (raylib:play-sound (sebundle-shoot0 sounds))
  (cancel-all #t)
  (bossinfo-active-attack-failed-set! bossinfo #f)
  (wait 60)
  (test-fairy-sp1 task enm)
  )

(define (test-fairy-sp1 task enm)
  (define bossinfo (enm-extras enm))
  (define keep-running
	(lambda () (and (positive? (enm-health enm))
					(positive? (bossinfo-remaining-timer bossinfo)))))
  (bossinfo-dummy-healthbars-set! bossinfo (immutable-vector))
  (declare-spell enm "Conjuring \"Eternal Meek\"" 1800 3000 2000000)
  (wait 120)
  (spawn-subtask "spam"
	(lambda (_task)
	  (loop-forever
	   (dotimes 5
		 (spawn-bullet 'small-ball-blue (enm-x enm) (enm-y enm)
					   5
					   (curry linear-step-forever (centered-roll game-rng pi)
							  5.0)))))
	keep-running task)
  (wait-while keep-running)
  (common-spell-postlude bossinfo enm))

(define (tick-bomb)
  (define (bomb-sweep-x-left-hitbox)
	(values (- bomb-sweep-x-left 40.0)
			+playfield-min-y+
			40
			+playfield-height+))

  (define (bomb-sweep-x-right-hitbox)
	(values bomb-sweep-x-right
			+playfield-min-y+
			40
			+playfield-height+))

  (define (bomb-sweep-y-up-hitbox)
	(values +playfield-min-x+
			(- bomb-sweep-y-up 40.0)
			+playfield-width+
			40))

  (define (bomb-sweep-y-down-hitbox)
	(values +playfield-min-x+
			bomb-sweep-y-down
			+playfield-width+
			40))
  (when (= bombing (- +bombing-max+ +bomb-initial-phase-length+))
	(raylib:play-sound (sebundle-oldvwoopfast sounds)))
  (when (<= bombing (- +bombing-max+ +bomb-initial-phase-length+))
	(let ([progress (ease-out-expo
					 (inexact (/ (- +bomb-noninitial-phase-length+ bombing)
								 +bomb-noninitial-phase-length+)))]
		  [bomb-sweep-x-left-max
		   (max +playfield-min-x+ (- initial-bomb-sweep-x-left 290))]
		  [bomb-sweep-x-right-max
		   (min +playfield-max-x+ (+ initial-bomb-sweep-x-right 290))]
		  [bomb-sweep-y-up-max
		   (max +playfield-min-y+ (- initial-bomb-sweep-y-up 365))]
		  [bomb-sweep-y-down-max
		   (min +playfield-max-y+ (+ initial-bomb-sweep-y-down 365))])
	  (set! bomb-sweep-x-left
			(lerp initial-bomb-sweep-x-left bomb-sweep-x-left-max progress))
	  (set! bomb-sweep-x-right
			(lerp initial-bomb-sweep-x-right bomb-sweep-x-right-max progress))
	  (set! bomb-sweep-y-up
			(lerp initial-bomb-sweep-y-up bomb-sweep-y-up-max progress))
	  (set! bomb-sweep-y-down
			(lerp initial-bomb-sweep-y-down bomb-sweep-y-down-max progress))))
  (let-values ([(xlx xly xlw xlh) (bomb-sweep-x-left-hitbox)]
			   [(xrx xry xrw xrh) (bomb-sweep-x-right-hitbox)]
			   [(yux yuy yuw yuh) (bomb-sweep-y-up-hitbox)]
			   [(ydx ydy ydw ydh) (bomb-sweep-y-down-hitbox)])
	(vector-for-each-truthy
	 (lambda (blt)
	   (when (bullet-active? blt)
		 (let ([x (bullet-x blt)]
			   [y (bullet-y blt)]
			   [hit-radius (bullet-hit-radius (bullet-type blt))])
		   (when (or (check-collision-circle-rec x y hit-radius
												 xlx xly xlw xlh)
					 (check-collision-circle-rec x y hit-radius
												 xrx xry xrw xrh)
					 (check-collision-circle-rec x y hit-radius
												 yux yuy yuw yuh)
					 (check-collision-circle-rec x y hit-radius
												 ydx ydy ydw ydh))
			 (cancel-bullet-with-drop blt 'small-piv)))))
	 live-bullets)
	(vector-for-each-truthy
	 (lambda (enm)
	   (let-values ([(x y w h) (enm-hurtbox enm)])
		 (when (or (check-collision-recs x y w h xlx xly xlw xlh)
				   (check-collision-recs x y w h xrx xry xrw xrh)
				   (check-collision-recs x y w h yux yuy yuw yuh)
				   (check-collision-recs x y w h ydx ydy ydw ydh))
		   (damage-enemy enm 35 #f))))
	 live-enm))
  (set! bombing (sub1 bombing)))

(define (tick-player)
  (when (fxpositive? bombing)
	(tick-bomb))
  (when (fxpositive? iframes)
	(set! iframes (sub1 iframes)))
  (when (fxpositive? respawning)
	(set! respawning (sub1 respawning)))
  (when (fxpositive? death-timer)
	(set! death-timer (sub1 death-timer))
	(when (zero? death-timer)
	  (kill-player)))

  ;; set the option positions
  (let ([progress (ease-out-cubic (/ focus-frames +max-focus-frames+))])
	(vector-set! option-xs 0
				 (lerp (- player-x 35) (- player-x 25) progress))
	(vector-set! option-ys 0
				 (lerp player-y (- player-y 18) progress))
	(vector-set! option-xs 1
				 (lerp (- player-x 17) (- player-x 10) progress))
	(vector-set! option-ys 1
				 (lerp (- player-y 30) (- player-y 35) progress))
	(vector-set! option-xs 2
				 (lerp (+ player-x 17) (+ player-x 10) progress))
	(vector-set! option-ys 2
				 (lerp (- player-y 30) (- player-y 35) progress))
	(vector-set! option-xs 3
				 (lerp (+ player-x 35) (+ player-x 25) progress))
	(vector-set! option-ys 3
				 (lerp player-y (- player-y 17) progress))))

(define (toggle-paused)
  (set! paused (not paused))
  (if paused
	  (let ()
		(raylib:play-sound (sebundle-pause sounds))
		(raylib:pause-music-stream ojamajo-carnival))
	  (raylib:resume-music-stream ojamajo-carnival)))

(define (handle-input)
  ;; is-key-down: Level-triggered
  ;; is-key-pressed, is-key-released: Edge-triggered
  (when (not paused)
	(set! focused-immediate (raylib:is-key-down key-left-shift))
	(if focused-immediate
		(when (fx< focus-frames +max-focus-frames+)
		  (set! focus-frames (fx1+ focus-frames)))
		(when (fx> focus-frames 0)
		  (set! focus-frames (fx1- focus-frames)))))
  (when (and (not paused) (zero? respawning))
	(when (zero? death-timer)
	  ;; don't allow moving between hit/death
	  (handle-player-movement))
	(when (raylib:is-key-down key-z)
	  (when (raylib:is-key-pressed key-z)
		(set! start-shot-frames frames))
	  (when (= 5 (mod (fx- frames start-shot-frames) 10))
		(vector-for-each
		 (lambda (x y)
		   (spawn-misc-ent (miscenttype needle) x y -10 0))
		 option-xs option-ys))
	  (when (zero? (mod (fx- frames start-shot-frames) 5))
		(let ([y (- player-y 20)])
		  (spawn-misc-ent (miscenttype mainshot) (- player-x 10) y
						  -10 0)
		  (spawn-misc-ent (miscenttype mainshot) (+ player-x 10) y
						  -10 0)
		  (raylib:play-sound (sebundle-playershoot sounds))))))
  
  (when (raylib:is-key-released key-z)
	(set! start-shot-frames -1))
  (when (raylib:is-key-pressed key-f1)
	(set! force-invincible (not force-invincible)))
  (when (raylib:is-key-pressed key-f2)
	(raylib:take-screenshot (string-append "screenshot-" (date-and-time)
										   ".png")))
  (when (raylib:is-key-pressed key-f3)
	(set! show-hitboxes (not show-hitboxes)))
  (when (raylib:is-key-pressed key-escape)
	(toggle-paused))
  (when (and
		 (raylib:is-key-pressed key-x)
		 (not paused)
		 (zero? bombing)
		 (zero? respawning)
		 (>= bomb-stock 1))
	(fail-current-attack)
	(set! death-timer 0)
	(set! bombing +bombing-max+)
	(set! iframes 180)
	(set! bomb-stock (sub1 bomb-stock))
	(autocollect-all-items)
	(raylib:play-sound (sebundle-spelldeclare sounds))
	(cancel-all #f)
	(set! bomb-sweep-x-left (- player-x 50.0))
	(set! initial-bomb-sweep-x-left bomb-sweep-x-left)
	(set! bomb-sweep-x-right (+ player-x 50.0))
	(set! initial-bomb-sweep-x-right bomb-sweep-x-right)
	(set! bomb-sweep-y-down (+ player-y 50.0))
	(set! initial-bomb-sweep-y-down bomb-sweep-y-down)
	(set! bomb-sweep-y-up (- player-y 50.0))
	(set! initial-bomb-sweep-y-up bomb-sweep-y-up))
  (when (raylib:is-key-pressed key-space)
	(values))
  (when (and (raylib:is-key-pressed key-left-bracket))
	(if (raylib:is-key-down key-left-shift)
		(decrease-music-volume)
		(decrease-sound-volume)))
  (when (and (raylib:is-key-pressed key-right-bracket))
	(if (raylib:is-key-down key-left-shift)
		(increase-music-volume)
		(increase-sound-volume)))
  (when (raylib:is-key-pressed key-period)
	(set! chapter-select (min (add1 chapter-select) 13)))
  (when (raylib:is-key-pressed key-comma)
	(set! chapter-select (max (sub1 chapter-select) 0)))
  (when (raylib:is-key-pressed key-r)
	(if paused
		(begin
		  (reset-state)
		  (reset-to 0)
		  (toggle-paused))
		(reset-to chapter-select))

	;; (set! frame-save-diff (- frames frame-save))
	;; (set! frame-save frames)
	)

  (when (raylib:is-key-pressed key-a)
	(when (> spline-editor-selected-position 0)
	  (set! spline-editor-selected-position (sub1 spline-editor-selected-position))))
  (when (raylib:is-key-pressed key-d)
	(set! spline-editor-selected-position (add1 spline-editor-selected-position)))
  (when (raylib:is-key-pressed key-s)
	(if (<= 0 spline-editor-selected-position (sub1 (vlen spline-editor-positions)))
		(vector-set! spline-editor-positions spline-editor-selected-position
					 (vec2 player-x player-y))
		(set! spline-editor-positions (vector-add spline-editor-positions
												  (vec2 player-x player-y)))))
  (when (and (raylib:is-key-pressed key-f)
			 (not (zero? (vlen spline-editor-positions))))
	(set! spline-editor-positions (vector-pop spline-editor-positions)))
  )

(define (handle-player-movement)
  (define left-pressed (raylib:is-key-down key-left))
  (define right-pressed (raylib:is-key-down key-right))
  (define up-pressed (raylib:is-key-down key-up))
  (define down-pressed (raylib:is-key-down key-down))
  (define dir (vec2 (fl+ (if left-pressed -1.0 0.0) (if right-pressed 1.0 0.0))
					(fl+ (if up-pressed -1.0 0.0) (if down-pressed 1.0 0.0))))
  (cond
   [(not (flzero? (v2x dir)))
	(set! player-dx-render (clamp (fx+ player-dx-render (exact (v2x dir))) -10 10))]
   [(< player-dx-render 0)
	(set! player-dx-render (fx1+ player-dx-render))]
   [(> player-dx-render 0)
	(set! player-dx-render (fx1- player-dx-render))])
  (when (not (and (flzero? (v2x dir)) (flzero? (v2y dir))))
	(let* ([speed (if focused-immediate 2.25 4.125)]
		   [velvec (v2* (v2unit dir) speed)]
		   [new-x (clamp (fl+ player-x (v2x velvec))
						 +playfield-min-x+ +playfield-max-x+)]
		   [new-y (clamp (fl+ player-y (v2y velvec))
						 +playfield-min-y+ +playfield-max-y+)])
	  (set! player-x new-x)
	  (set! player-y new-y)))
  (when (< player-y +poc-y+)
	(autocollect-all-items)))

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

(define (draw-player textures)
  (define-values (render-player-x render-player-y)
	(get-player-render-pos))
  ;; player sprite (todo: directional moving sprites)
  (raylib:draw-texture-rec
   (txbundle-reimu textures)
   (cond
	[(fxzero? player-dx-render)
	 (let ([x-texture-index (truncate (mod (/ frames 9) 8))])
	   (make-rectangle (* 32.0 x-texture-index) 0.0 32.0 48.0))]
	[(fx<= 1 player-dx-render 3)
	 (make-rectangle 32.0 96.0 32.0 48.0)]
	[(fx<= 4 player-dx-render 7)
	 (make-rectangle 64.0 96.0 32.0 48.0)]
	[(fx> player-dx-render 7)
	 (let ([x-texture-index (+ 3 (truncate (mod (/ frames 9) 5)))])
	   (make-rectangle (* 32.0 x-texture-index) 96.0 32.0 48.0))]
	[(fx<= -3 player-dx-render -1)
	 (make-rectangle 32.0 48.0 32.0 48.0)]
	[(fx<= -7 player-dx-render -4)
	 (make-rectangle 64.0 48.0 32.0 48.0)]
	[else
	 (let ([x-texture-index (+ 3 (truncate (mod (/ frames 9) 5)))])
	   (make-rectangle (* 32.0 x-texture-index) 48.0 32.0 48.0))])
   (vec2 (- render-player-x 16.0) (- render-player-y 24.0))
   (if (and (player-invincible?) (< (mod frames 4) 2))
	   invincible-flash
	   -1))

  (when (positive? death-timer)
	(let ([radius (lerp 80.0 0.0 (/ (- +deathbomb-time+ death-timer)
									+deathbomb-time+))])
	  (raylib:draw-circle-v render-player-x render-player-y radius #xff000088)))

  ;; options
  (vector-for-each
   (lambda (x y)
	 (define render-x (+ x +playfield-render-offset-x+))
	 (define render-y (+ y +playfield-render-offset-y+))
	 (draw-sprite-with-rotation
	  textures 'option (mod (* frames 4) 360)
	  render-x render-y -1))
   option-xs option-ys)

  (let ([focus-progress (/ focus-frames +max-focus-frames+)])
	(raylib:draw-circle-lines-v
	 render-player-x render-player-y
	 (lerp vacuum-radius-unfocused vacuum-radius-focused
		   focus-progress)
	 (packcolor 255 255 255 (eround (lerp 16 128 focus-progress)))))

  (when show-hitboxes
	(raylib:draw-circle-v render-player-x render-player-y graze-radius
						  (packcolor 0 228 48 255))
	(raylib:draw-circle-v render-player-x render-player-y hit-radius
						  red)))

(define (draw-boss-hud enm textures fonts)
  (define bossinfo (enm-extras enm))
  (define remaining-timer (bossinfo-remaining-timer bossinfo))
  (define elapsed-frames (fx- (bossinfo-total-timer bossinfo)
							  remaining-timer))
  (define spname (bossinfo-active-spell-name bossinfo))
  (define (render-dummy-healthbars)
	(define healthbars (bossinfo-dummy-healthbars bossinfo))
	(let loop ([x (+ +playfield-render-offset-x+ +playfield-min-x+ 5)]
			   [i 0])
	  (if (= i (vlen healthbars))
		  x
		  (let ([hb (vnth healthbars i)])
			(raylib:draw-rectangle-gradient-v
			 x
			 (+ +playfield-render-offset-y+ +playfield-min-y+)
			 (dummy-healthbar-width hb) 5
			 (dummy-healthbar-top-color hb)
			 (dummy-healthbar-bottom-color hb))
			(loop (+ x (dummy-healthbar-width hb) (dummy-healthbar-post-padding hb))
				  (add1 i))))))
  ;; TODO(stack the names vertically if there's multiple bosses)
  (raylib:draw-text-ex (fontbundle-bubblegum fonts)
					   (bossinfo-name bossinfo)
					   (+ +playfield-min-render-x+ 5)
					   (+ +playfield-min-render-y+ 5)
					   12.0 0.0 (bossinfo-name-color bossinfo))
  (let ([dummy-healthbars-end (render-dummy-healthbars)]
		[cur-atk-max-health (bossinfo-max-health bossinfo)])
	(when (or (= -1 cur-atk-max-health) (> cur-atk-max-health 0))
	  (raylib:draw-rectangle-gradient-v
	   dummy-healthbars-end
	   (+ +playfield-render-offset-y+ +playfield-min-y+)
	   (eround (* (- (- +playfield-max-render-x+ 20.0) dummy-healthbars-end)
				  (if (= -1 cur-atk-max-health)
					  (/ remaining-timer (bossinfo-total-timer bossinfo))
					  (/ (enm-health enm) cur-atk-max-health))))
	   5
	   #xf5f5f5ff
	   #x808080ff)))
  (unless (fxzero? remaining-timer)
	(raylib:draw-text-ex (fontbundle-sharetechmono fonts)
						 (format "~2,'0d"
								 (exact (ceiling
								  (/ (bossinfo-remaining-timer bossinfo) 60.0))))
						 (- +playfield-max-render-x+ 18)
						 (- +playfield-min-render-y+ 0)
						 20.0 0.0
						 (cond
						  [(fx<= remaining-timer 300) #xf08080ff]
						  [(fx<= remaining-timer 600) #xffb6c1ff]
						  [else -1]))
	(when spname
	  (let*-values ([(width height) (raylib:measure-text-ex
									 (fontbundle-cabin fonts)
									 spname 18.0 0.5)]
					[(spx) (+ +playfield-render-offset-x+
							  -5.0
							  (if (fx<= elapsed-frames 30)
								  (+ +playfield-max-x+
									 (lerp 0.0 (fl- width)
										   (ease-out-cubic (/ elapsed-frames 30))))
								  (+ +playfield-max-x+ (fl- width))))]
					[(spy) (+ +playfield-render-offset-y+
							  (cond
							   [(fx<= elapsed-frames 45)
								(- +playfield-max-y+ height 15.0)]
							   [(fx<= 45 elapsed-frames 90)
								(lerp
								 (- +playfield-max-y+ height 15.0)
								 (+ +playfield-min-y+ 15.0)
								 (ease-out-cubic (/ (fx- elapsed-frames 45) 45.0)))]
							   [else (+ +playfield-min-y+ 15.0)]))]
					[(bonus) (calculate-spell-bonus bossinfo)])
		;; TODO: scissor when outside of playfield
		(raylib:draw-texture-pro
		 (txbundle-boss-ui textures)
		 (make-rectangle 0.0 0.0 256.0 36.0)
		 (make-rectangle (- +playfield-max-render-x+ 256.0) (- spy 5.0) 256.0 36.0)
		 v2zero 0.0 -1)
		(raylib:draw-text-ex
		 (fontbundle-cabin fonts) spname
		 spx spy
		 18.0 0.5 -1)
		(raylib:draw-text-ex
		 (fontbundle-sharetechmono fonts) (format "Bonus: ~:d" bonus)
		 spx (+ spy height 5.0)
		 15.0 0.0 -1))))
  (draw-sprite textures 'enemy-indicator
			   (+ +playfield-render-offset-x+
				  (clamp (enm-x enm) +playfield-min-x+ +playfield-max-x+))
			   (inexact (+ +playfield-max-y+ +playfield-render-offset-y+))
			   #xffffffc0))

(define (draw-hud textures fonts)
  (raylib:draw-texture (txbundle-hud textures) 0 0 #xffffffff)
  (raylib:draw-text-ex
   (fontbundle-bubblegum fonts)
   (format "Score: ~:d" current-score)
   440 15 24.0 0.0 -1)
  (raylib:draw-text-ex
   (fontbundle-bubblegum fonts)
   "Life"
   440 45
   24.0 0.0 #xFF69FCFF)
  (raylib:draw-text-ex
   (fontbundle-bubblegum fonts)
   "Bomb"
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
  (when-let ([boss (first-boss)])
	(draw-boss-hud boss textures fonts))

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
  (raylib:draw-text (format "VOL: ~d ~d"
							(cdr (assq 'music-vol config))
							(cdr (assq 'sfx-vol config)))
					440 175 18 -1)
  (raylib:draw-text (format "SPLED: ~d of [0, ~d]" spline-editor-selected-position
							(sub1 (vlen spline-editor-positions)))
					440 200 18 -1)
  (raylib:draw-text (format "FRSAV: ~d (~d)" frame-save frame-save-diff)
					440 225 18 -1)
  (raylib:draw-text (format "CHAP: ~d / GOTO: ~d" current-chapter chapter-select)
					440 250 18 -1)
  (raylib:draw-text (format "X: ~,2f / Y: ~,2f" player-x player-y)
					440 275 18 -1)
  (raylib:draw-text (format "MEM: ~,2f MiB"
							(/ (bytes-allocated)
							   (* 1024.0 1024.0)))
					440 300 18 -1)
  (raylib:draw-text (format "FRAME: ~d" frames)
					440 325 18 -1)
  (raylib:draw-text (format "MISC: ~d / PART: ~d"
							(vector-popcnt live-misc-ents)
							(vector-popcnt live-particles))
					440 350 18 -1)
  (raylib:draw-text (format "ENM: ~d" (vector-popcnt live-enm))
					440 375 18 -1)
  (raylib:draw-text (format "BLT: ~d / LFT: ~d"
							(vector-popcnt live-bullets)
							(fx1- next-bullet-id))
					440 400 18 -1)
  (raylib:draw-text (format "TASK: ~d" (task-count))
					440 425 18 -1)
  (raylib:draw-fps 440 450))

(define bg1-scroll 0.0)
(define bg2-scroll 0.0)
(define bg3-scroll 0.0)
(define (background-acceleration frames)
  ;; todo: make this more data driven
  (cond
   [(fx< frames 3250)
	(values 0.5 1.0 1.5)]
   [(fx< 3250 frames 3600)
	(let ([progress (/ (fx- frames 3250)
					   (fx- 3600 3250))])
	  (values
	   (lerp 0.5 1.8 progress)
	   (lerp 1.0 2.0 progress)
	   (lerp 1.5 2.5 progress)))]
   [(fx< 3600 frames 5500)
	(values 1.8 2.0 2.5)]
   [(fx< 5500 frames 6300)
	(let ([progress (/ (fx- frames 5500)
						(fx- 6300 5500))])
	  (values
	   (lerp 1.8 0.5 progress)
	   (lerp 2.0 1.0 progress)
	   (lerp 2.5 1.5 progress)))]
   [(fx< 6300 frames 9000)
	(values 0.5 1.0 1.5)]
   [(fx< 9000 frames 9500)
	(let ([progress (/ (fx- frames 9000)
					   (fx- 9500 9000))])
	  (values
	   (lerp 0.5 1.8 progress)
	   (lerp 1.0 2.0 progress)
	   (lerp 1.5 2.5 progress)))]
   [(fx< 9500 frames 11070)
	(values 1.8 2.0 2.5)]
   [(fx< 11070 frames 11550)
	(values 0.4 0.9 1.4)]
   [(fx< 11550 frames 11850)
	(let ([progress (/ (fx- frames 11550)
					   (fx- 11850 11550))])
	  (values
	   (lerp 0.4 1.8 progress)
	   (lerp 0.9 2.0 progress)
	   (lerp 1.4 2.5 progress)))]
   [(fx< 11850 frames 12800)
	(values 1.8 2.0 2.5)]
   [(fx< 12800 frames 13000)
	(let ([progress (/ (fx- frames 12800)
					   (fx- 13200 12800))])
	  (values
	   (lerp 1.8 0.5 progress)
	   (lerp 2.0 1.0 progress)
	   (lerp 2.5 1.5 progress)))]
   [else (values 0.5 1.0 1.5)]))

(define bullet-cc (make-cost-center))
(define miscent-cc (make-cost-center))

(define background-draw-bounds
  ;; x is integer multiple of texture width to prevent stretching
  ;; y isn't but the stretching isn't too noticeable so it's ok
  (make-rectangle 0.0 0.0 512.0 480.0))
(define (do-render-all textures fonts)
  (raylib:clear-background #x000000ff) ;;#x42024aff) ;; todo: some variability :D
  (unless paused
	(let-values ([(bg1-vel bg2-vel bg3-vel) (background-acceleration frames)])
	  (set! bg1-scroll (fl- bg1-scroll bg1-vel))
	  (set! bg2-scroll (fl- bg2-scroll bg2-vel))
	  (set! bg3-scroll (fl- bg3-scroll bg3-vel))))
  (raylib:draw-texture-pro (txbundle-bg1 textures)
						   (make-rectangle 0.0 bg1-scroll 256.0 224.0)
						   background-draw-bounds
						   v2zero 0.0 #xc0c0c0ff)
  (raylib:draw-texture-pro (txbundle-bg2 textures)
						   (make-rectangle 0.0 bg2-scroll 256.0 224.0)
						   background-draw-bounds
						   v2zero 0.0 #xc0c0c0ff)
  (raylib:draw-texture-pro (txbundle-bg3 textures)
						   (make-rectangle 0.0 bg3-scroll 256.0 224.0)
						   background-draw-bounds
						   v2zero 0.0 #xc0c0c0ff)
  ;; (raylib:draw-texture-pro (txbundle-bg4 textures)
  ;; 						   (make-rectangle 0.0 (mod (* frames -0.5) 224.0) 256.0 224.0)
  ;; 						   background-draw-bounds
  ;; 						   v2zero 0.0 -1)
  (when show-hitboxes
	(raylib:draw-line (+ +playfield-render-offset-x+ +playfield-min-x+)
					  (+ +playfield-render-offset-y+ +poc-y+)
					  (+ +playfield-render-offset-x+ +playfield-max-x+)
					  (+ +playfield-render-offset-y+ +poc-y+)
					  -1))

  (let ([sorted-bullets (vector-sort
						 (lambda (a b)
						   (cond
							[(not a) #t]
							[(not b) #f]
							[else (fx< (bullet-id a) (bullet-id b))]))
						 live-bullets)])
	;; lasers go under enemies, all other bullets on top
	(draw-lasers textures sorted-bullets)
	(draw-enemies textures)
	(draw-player textures)
	(with-cost-center miscent-cc
					  (thunk (draw-misc-ents textures)))
	(with-cost-center bullet-cc
					  (thunk (draw-bullets textures sorted-bullets))))
  (draw-particles textures fonts)

  ;; focus sigil. Done here after the bullets because we want the player hitbox
  ;; to render on top of big bullets like bubbles, and ryannlib has the hitbox and
  ;; focus sigil combined in one texture. We should consider separating the two and
  ;; drawing them separately, because having the sigil show up on top of bullets too
  ;; is kinda weird.
  (let-values ([(render-player-x render-player-y)
				(get-player-render-pos)]
			   [(focus-sigil-strength) (/ focus-frames +max-focus-frames+)])
	(when (> focus-sigil-strength 0)
	  (raylib:with-matrix
	   (raylib:translatef render-player-x render-player-y 0.0) ;; move to where we are
	   (raylib:rotatef (mod frames 360.0) 0.0 0.0 1.0) ;; spin
	   (draw-sprite textures 'focus-sigil
					0.0 0.0 ;; manually translated to final position above
					(packcolor 255 255 255 (eround (* 255 focus-sigil-strength)))))))
  
  (when (positive? bombing)
	(draw-bomb textures))

  (when (and paused (< (mod true-frames 60) 30))
	(raylib:draw-text-ex
	 (fontbundle-bubblegum fonts)
	 "Paused"
	 175 150 32.0 0.0 (packcolor 200 122 255 255)))
  (draw-hud textures fonts)

  (let ([render-positions (vector-map
						   (lambda (p) (v2+ p +playfield-render-offset+))
						   spline-editor-positions)])
	(when (not (zero? (vlen render-positions)))
	  (raylib:draw-spline-bezier-cubic
	   (truncate-to-whole-spline render-positions) 5.0 red))
	(vector-for-each-indexed
	 (lambda (i p)
	   (when p
		 (raylib:draw-circle-v (v2x p) (v2y p) 10.0
							   (if (= i spline-editor-selected-position)
								   #x006400ff
								   green))
		 (raylib:draw-text
		  (format "~d: ~d ~d"
				  i
				  (eround (v2x (vnth spline-editor-positions i)))
				  (eround (v2y (vnth spline-editor-positions i))))
		  (eround (v2x p)) (eround (v2y p)) 10 -1)))
	 render-positions)))

(define (render-all render-texture render-texture-inner textures fonts)
  (raylib:begin-texture-mode render-texture)
  (do-render-all textures fonts)
  (raylib:end-texture-mode)

  (raylib:begin-drawing)
  (raylib:clear-background #x000000ff)
  (raylib:draw-texture-pro
   render-texture-inner
   ;; why negative height?
   ;; raylib is origin-at-top-left, and all our code from before render targets
   ;; started being used assumes that. However, opengl is origin-at-bottom-left,
   ;; so drawing to a render target then immediately blitting that to the
   ;; framebuffer results in an inverted image. Supplying a negative height
   ;; here flips it again to correct for that. It's unfortunate that in
   ;; trying to paper over an OpenGL design choice, Raylib actually introduces
   ;; another mismatch.
   (make-rectangle 0.0 0.0 640.0 -480.0)
   (make-rectangle 0.0 0.0 1280.0 960.0) v2zero 0.0 -1)
  (raylib:end-drawing))

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

(define (midboss-control task enm)
  (define bossinfo (enm-extras enm))
  (define keep-running
	(lambda () (positive? (bossinfo-remaining-timer bossinfo))))
  (ease-to values 0.0 100.0 20 enm)
  (raylib:play-sound (sebundle-longcharge sounds))
  (wait 40)

  (enm-addflags enm (enmflags invincible))
  (declare-spell enm "Conjuring \"Eternal Meek\"" 1540 -1 2000000)
  (cancel-all #f)
  (wait 80)
  (raylib:play-sound (sebundle-shortcharge sounds))
  (wait 60)
  ;; TODO: spawner task moving up and down (on the sides probably?)
  ;; spawns decorative bullets + some rings that delay aim at the player
  ;; maybe vertical/box streaming is the idea?
  ;; if box streaming then spawners would move around all edges instead of just
  ;; up/down
  (spawn-subtask "spam"
	(lambda (_task)
	  (loop-forever
	   (dotimes 3
		 (spawn-bullet 'small-ball-blue (enm-x enm) (enm-y enm)
					   5
					   (curry linear-step-forever (centered-roll game-rng pi)
							  5.0)))))
	keep-running task)
  (wait-while keep-running)
  (enm-clrflags enm (enmflags invincible))
  (common-spell-postlude bossinfo enm)
  (common-boss-postlude bossinfo enm))

(define (chapter4 task)
  (set! current-chapter 4)
  (let ([enm (spawn-enemy (enmtype boss-doremi) 100.0 -100.0 500 midboss-control
						  '((bomb . 1) (point . 30))
						  (thunk #f))]
		[bossinfo (make-bossinfo "Harukaze Doremi" #xff7fbcff
								 (make-flvector +boss-lazy-spellcircle-context+ 0.0)
								 (make-flvector +boss-lazy-spellcircle-context+ 100.0)
								 #t #f #f #f 0 0 0 (immutable-vector))])
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
				 (define turn-dir (torad (if (fxeven? layer) 4.0 -4.0)))
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
				 (define turn-dir (torad (if (fxeven? layer) 4.0 -4.0)))
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
  (delete-enemy enm)
  )

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
		   (delete-enemy enm)
		   ))))
	)
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
	  (let* ([center-blt
			  (spawn-bullet
			   'big-star-orange x y 5
			   (lambda (_) #f))]
			 [ring-blts (map (lambda (_)
							   (spawn-bullet 'small-star-yellow
										     x y 5
											 (lambda (_) #f)))
							 (iota 10))]
			 [facing (facing-player x y)])
		(raylib:play-sound (sebundle-shoot0 sounds))
		(spawn-task "ch9w2-ring-controller"
		  (lambda (task)
			(let loop ([i 0])
			  (linear-step facing 5.0 center-blt)
			  (let ([r (flmin (fl* (inexact i) 1.5) 30.0)])
				(for-each-indexed
				 (lambda (j sub)
				   (bullet-x-set!
					sub
					(fl+ (bullet-x center-blt)
						 (fl* r (flcos (fl* (torad 36.0) (inexact j))))))
				   (bullet-y-set!
					sub
					(fl+ (bullet-y center-blt)
						 (fl* r (flsin (fl* (torad 36.0) (inexact j)))))))
				 ring-blts))
			  (yield)
			  (when (fx< i 240)
				(loop (fx1+ i))))
			(delete-bullet center-blt)
			(for-each delete-bullet ring-blts))
		  (constantly #t)))))
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
  (let ([drops '((point . 15))])
	(spawn-enemy (enmtype big-fairy) -90.0 -20.0 400 ch9-w2 drops)
	(spawn-enemy (enmtype big-fairy) 0.0 -30.0 400 ch9-w2 drops)
	(spawn-enemy (enmtype big-fairy) 90.0 -20.0 400 ch9-w2 drops))
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
  (spawn-subtask "decoration"
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
  (spawn-enemy 'green-fairy
			   -200.0 50.0
			   100 (curry ch10-w5 #f))
  (spawn-enemy 'green-fairy
			   200.0 50.0
			   100 (curry ch10-w5 #t))
  (wait 100)
  (spawn-enemy 'green-fairy
			   -200.0 130.0
			   100 (curry ch10-w5 #f))
  (spawn-enemy 'green-fairy
			   200.0 130.0
			   100 (curry ch10-w5 #t))
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
  (declare-spell enm "\"???\"" 720 -1 5000000)
  (cancel-all #f)
  (wait-while keep-running)
  (common-spell-postlude bossinfo enm)
  (common-boss-postlude bossinfo enm))

(define (chapter11 task)
  (set! current-chapter 11)
  ;; midspell 2, micro spinning?
  (let ([enm (spawn-enemy (enmtype boss-doremi) 100.0 -100.0 500 midboss2-control
						  '((life . 1) (point . 50))
						  (thunk #f))]
		[bossinfo (make-bossinfo "Harukaze Doremi" #xff7fbcff
								 (make-flvector +boss-lazy-spellcircle-context+ 0.0)
								 (make-flvector +boss-lazy-spellcircle-context+ 100.0)
								 #t #f #f #f 0 0 0 (immutable-vector))])
	(enm-extras-set! enm bossinfo))
  (wait-until (thunk (>= frames 11785)))
  (chapter12 task))

(define (ch12-small-fairy right-side task enm)
  (spawn-subtask "shoot"
	(lambda (task)
	  (interval-loop 10
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
  ;; TODO: just a couple waves of self spawning bullets.
  ;; Last one has absolute aim towards middle?
  (wait-until (thunk (>= frames 13000)))
  (chapter13 task))

(define (chapter13 task)
  (set! current-chapter 13)
  (loop-forever))

(define stage-driver-task #f)

(define (reset-to chapter)
  ;; we do it like this instead of putting the functions directly in an
  ;; alist/vector because otherwise we don't pick up live reloads
  (define-values (func timestamp)
	(case chapter
	  [(0) (values chapter0 0)] [(1) (values chapter1 870)]
	  [(2) (values chapter2 1645)] [(3) (values chapter3 2425)]
	  [(4) (values chapter4 3500)] [(5) (values chapter5 5200)]
	  [(6) (values chapter6 6339)] [(7) (values chapter7 6725)]
	  [(8) (values chapter8 7497)] [(9) (values chapter9 8284)]
	  [(10) (values chapter10 9392)] [(11) (values chapter11 10960)]
	  [(12) (values chapter12 11785)] [(13) (values chapter13 13000)]))
  (vector-fill! live-bullets #f)
  (vector-fill! live-enm #f)
  (vector-fill! live-misc-ents #f)
  (vector-fill! live-particles #f)
  (kill-all-tasks)
  (set! stage-driver-task (spawn-task "stage driver" func (constantly #t)))
  (set! frames timestamp)
  (raylib:seek-music-stream ojamajo-carnival (inexact (/ frames 60.0))))

(define (main)
  (raylib:set-trace-log-level 4) ;; WARNING or above
  (raylib:init-window 1280 960 "thdawn")
  (raylib:set-target-fps 60)
  (raylib:set-exit-key 0)
  (set! config (read-config))
  (load-audio)
  (load-sfx)
  
  (let* ([textures (load-textures)]
		 [fonts (load-fonts)]
		 [render-texture (raylib:load-render-texture 640 480)]
		 [render-texture-inner (raylib:render-texture-inner render-texture)])
	(raylib:play-music-stream ojamajo-carnival)
	(set! stage-driver-task
		  (spawn-task "stage driver" chapter0 (constantly #t)))
	(dynamic-wind
	  (thunk #f)
	  (lambda ()
		(do [] [(raylib:window-should-close)]
		  (handle-input)
		  (raylib:update-music-stream ojamajo-carnival)
		  (unless paused
			(tick-player)
			(vector-for-each-truthy
			 despawn-out-of-bound-bullet
			 live-bullets)
			(pretick-enemies)
			(run-tasks)
			(posttick-enemies)
			(tick-misc-ents)
			(tick-particles)
			(process-collisions))
		  (render-all render-texture render-texture-inner textures fonts)
		  (unless paused
			(set! frames (fx1+ frames)))
		  (set! true-frames (fx1+ true-frames))
		  #;(when (fxzero? (fxmod true-frames 180))
			(display (format "B ~,2f || "
							 (/ (cost-center-allocation-count bullet-cc)
								(* 1024.0 1024.0))))
			(display (format "M ~,2f || "
							 (/ (cost-center-allocation-count miscent-cc)
								(* 1024.0 1024.0))))
			(reset-cost-center! bullet-cc)
			(reset-cost-center! miscent-cc)
			(newline))
		  ))
	  (lambda ()
		(unload-fonts fonts)
		(unload-textures textures)
		(unload-sfx)
		(unload-audio)
		(raylib:close-window)))))

(define (reset-state)
  (set! iframes 180)
  (set! frames 0)
  ;; Clear everything out because they may have obsolete data
  (vector-fill! live-bullets #f)
  (vector-fill! live-enm #f)
  (vector-fill! live-misc-ents #f)
  (vector-fill! live-particles #f)
  (kill-all-tasks)
  (set! life-stock 2)
  (set! bomb-stock 3)
  (set! item-value 10000)
  (set! current-score 0)
  (set! bombing 0)
  (set! death-timer 0)
  (set! graze 0))

(define (debug-launch)
  (reset-state)
  (set! paused #f)
  (fork-thread main))

(define (main-with-backtrace)
  (define (print-stacktrace e)
	(define ins (inspect/object (condition-continuation e)))
	(define depth (ins 'depth))
	(display "Crash! Please report this to the developer along with the version information.\n")
	(display "Bytes Allocated: ")
	(display (bytes-allocated))
	(newline)
	(when (message-condition? e)
	  (display "Message: ")
	  (display (condition-message e))
	  (newline))
	(display "\nBacktrace:\n")
	(do [(i 0 (fx1+ i))]
		[(fx= i depth)]
	  (let* ([frame (ins 'link* i)]
			 [source-obj (frame 'source-object)])
		(display (or ((frame 'code) 'name) "anonymous"))
		(when source-obj
		  (display " @ ")
		  (display source-obj))
		;; todo: still maybe want the 'source, but truncated?
		(newline))))
  (guard (e [(continuation-condition? e)
			 (print-stacktrace e)])
	(main)))

(scheme-start
 (lambda _ (main-with-backtrace)))
