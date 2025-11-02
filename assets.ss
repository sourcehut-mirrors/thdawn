(define-record-type sebundle
  (fields
   spellcapture spelldeclare
   longcharge shortcharge
   enmdie bossdie
   playerdie playershoot
   shoot0 shoot1 shoot2
   extend graze bell
   oldvwoopfast oldvwoopslow
   pause menuselect menuback
   timeout timeoutwarn item damage0 damage1
   dropbomb droplife
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
  (let ([vol-flt (inexact (/ (assqdr 'sfx-vol config) 100.0))])
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
				"se_pause.wav" "se_select00.wav" "se_cancel00.wav"
				"se_timeout.wav" "se_timeout2.wav"
				"se_item00.wav" "se_damage00.wav" "se_damage01.wav"
				"se_bonus2.wav" "se_bonus.wav"
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

(define-record-type musbundle
  (fields
   ojamajo-wa-koko-ni-iru
   ojamajo-carnival
   naisho-yo-ojamajo
   lupinasu-no-komoriuta-piano)
  (sealed #t))
(define music #f)
(define (each-music proc)
  (define rtd (record-type-descriptor musbundle))
  (define num-music (vlen (record-type-field-names rtd)))
  (when sounds
	(do [(i 0 (1+ i))]
		[(>= i num-music)]
	  (proc ((record-accessor rtd i) music)))))
(define (update-music-volumes)
  (let ([vol-flt (inexact (/ (assqdr 'music-vol config) 100.0))])
	(each-music (lambda (mus) (raylib:set-music-volume mus vol-flt)))))
(define (load-music)
  (set! music
		(apply
		 make-musbundle
		 (map (lambda (file) (raylib:load-music-stream
							  (string-append "assets/bgm/" file)))
			  '("ojamajo_wa_kokoni_iru.mp3" "ojamajo_carnival.wav"
				"naisho_yo_ojamajo.mp3" "lupinasu_no_komoriuta_piano.mp3"))))
  (update-music-volumes))
(define (unload-music)
  (each-music raylib:unload-music-stream)
  (set! music #f))
(define (increase-music-volume)
  (define pair (assq 'music-vol config))
  (when (< (cdr pair) 100)
	(set-cdr! pair (+ (cdr pair) 5))
	(save-config config)
	(update-music-volumes)))
(define (decrease-music-volume)
  (define pair (assq 'music-vol config))
  (when (> (cdr pair) 0)
	(set-cdr! pair (- (cdr pair) 5))
	(save-config config)
	(update-music-volumes)))
(define current-music #f)
(define (play-music m)
  (when current-music
	(raylib:stop-music-stream current-music))
  (set! current-music m)
  (raylib:play-music-stream m))

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
