;; All symbols declared from now on are in the thdawn namespace
(in-package :thdawn)

(defvar frames 0 "Number of frames the current stage has been running")

;; Try changing me, then updating the game live in the REPL!
(defparameter bgcolor :black)

;; To ease in live reloading while I still don't understand how multi file projects work,
;; everything's still gonna just be in one file (lol). Good enough for a game jam game.
;; Eventually we should move these out to other files and figure out how to load them
;; probably from each other.

;; Bullet system. Uses SOA/separate dense typed arrays for performance
(defconstant NUM-BULLETS 512)
(defvar bullet-types
  (make-array NUM-BULLETS :element-type 'keyword :initial-element :none)
  "bullet types, :none means this slot is empty and represents no bullet")
(defvar bullet-colors
  (make-array NUM-BULLETS :element-type 'keyword :initial-element :white)
  "bullet colors")
(defvar bullet-xs
  (make-array NUM-BULLETS :element-type 'float :initial-element 0.0)
  "bullet x positions")
(defvar bullet-ys
  (make-array NUM-BULLETS :element-type 'float :initial-element 0.0)
  "bullet y positions")
(defvar bullet-xvs
  (make-array NUM-BULLETS :element-type 'float :initial-element 0.0)
  "bullet x velocities")
(defvar bullet-yvs
  (make-array NUM-BULLETS :element-type 'float :initial-element 0.0)
  "bullet y velocities")
(defvar bullet-facing
  (make-array NUM-BULLETS :element-type 'float :initial-element 0.0)
  "bullet facing (degrees). For rendering only. Use velocity for movement.")
(defvar bullet-control
  (make-array NUM-BULLETS :element-type '(or null function) :initial-element nil)
  "Control function guiding this bullet's movement, called every frame.")
(defvar bullet-extras
  (let ((result (make-array NUM-BULLETS :element-type '(or null hash-table) :initial-element nil)))
	(dotimes (i NUM-BULLETS)
	  (setf (aref result i) (make-hash-table :size 4)))
	result)
  "Auxiliary hashtable of data any bullets need to store. If something's very commonly used across many bullets, it should get its own array.")

;; Common bullet control functions
(defun bullet-control-linear (id)
  "A simple control function that advances the bullet's position according to its (fixed) velocity"
  (incf (aref bullet-xs id) (aref bullet-xvs id))
  (incf (aref bullet-ys id) (aref bullet-yvs id)))

(defun bullet-control-accelerate (id)
  "Simple control function that advances the bullet's position according to its velocity, and applies an acceleration factor to its velocity
   Requires keys :xa and :ya to be present in the extra data hashtable, or else no acceleration is applied."
  ;; todo: acceleration common enough to be its own array instead of in extradata?
  (incf (aref bullet-xs id) (aref bullet-xvs id))
  (incf (aref bullet-ys id) (aref bullet-yvs id))
  (let ((data (aref bullet-extras id)))
	(incf (aref bullet-xvs id) (get :xa data 0))
	(incf (aref bullet-yvs id) (get :ya data 0))))

;; TODO: Others common control functions we might want (acceleration, deceleration, etc.)

(defun spawn-bullet (type x y xv yv facing control-function)
  ;; todo: if linear scan for a free slot becomes a concern, can consider implementing a next-fit style pointer, or using a bit-vector
  (let ((id (position :none bullet-types)))
	(unless id
	  (error "No more open bullet slots! Either bullets are not being killed properly or the pattern is too complex"))
	(setf (aref bullet-types id) type)
	(setf (aref bullet-xs id) x)
	(setf (aref bullet-ys id) y)
	(setf (aref bullet-xvs id) xv)
	(setf (aref bullet-yvs id) yv)
	(setf (aref bullet-facing id) facing)
	(setf (aref bullet-control id) control-function)
	id))

(defun delete-bullet (id)
  (when (eq :none (aref bullet-types id))
	(error "deleting inactive bullet slot"))
  ;; no need to clean others as they'll be lazily filled by the next spawn call
  (setf (aref bullet-types id) :none)
  (setf (aref bullet-control id) nil)
  (clrhash (aref bullet-extras id)))

;; [31-416] x bounds of playfield in the hud texture
;; [15-463] y bounds of playfield in the hud texture
;; idea is to have logical game x in [-192, 192] (192 is (416-31)/2, roughly), and y in [0, 448] 
;; logical game 0, 0 is at gl (31+(416-31)/2), 15
;; offset logical coords by 223, 15 to get to GL coords for render render
(defconstant playfield-render-offset-x 223)
(defconstant playfield-render-offset-y 15)
(defconstant playfield-min-x -192)
(defconstant playfield-min-y 0)
(defconstant playfield-max-x 192)
(defconstant playfield-max-y 448)
(defconstant oob-bullet-despawn-fuzz 10)

(defun despawn-out-of-bound-bullet (id)
  (let ((type (aref bullet-types id))
		(x (aref bullet-xs id))
		(y (aref bullet-ys id)))
	;; tolerate this, we could be calling this after a bullet's control function kills
	;; itself
	(when (and (not (eq :none type))
			   (or (> x (+ playfield-max-x oob-bullet-despawn-fuzz))
				   (< x (- playfield-min-x oob-bullet-despawn-fuzz))
				   (< y (- playfield-min-y oob-bullet-despawn-fuzz))
				   (> y (+ playfield-max-y oob-bullet-despawn-fuzz))))
	  (delete-bullet id))))

(defun tick-bullets ()
  (loop for id from 0
		for type across bullet-types
		do (when (not (eq :none type))
			 (funcall (aref bullet-control id) id)
			 (despawn-out-of-bound-bullet id))))

(defun draw-bullets (textures)
  (loop
	for id from 0
	for type across bullet-types
	for x across bullet-xs
	for y across bullet-ys
	for render-x = (+ x playfield-render-offset-x)
	for render-y = (+ y playfield-render-offset-y)
	do (case type
		 (:pellet-white
		  (draw-sprite textures :pellet-white render-x render-y :raywhite))
		 (:none t))))

;; Non-boss enemies. Uses SOA/separate dense typed arrays for performance
;; NB: Why no velocity? Because most enemies are going to be moving in different, custom
;; ways, so a single velocity system applied to all the enemies at the same time doesn't
;; make sense.
(defconstant NUM-ENM 256)
(defvar enm-types
  (make-array NUM-ENM :element-type 'keyword :initial-element :none)
  "enemy types, :none means this slot is empty and represents no enemy")
(defvar enm-xs
  (make-array NUM-ENM :element-type 'float :initial-element 0.0)
  "enemy x positions")
(defvar enm-ys
  (make-array NUM-ENM :element-type 'float :initial-element 0.0)
  "enemy y positions")
(defvar enm-facing
  (make-array NUM-ENM :element-type 'keyword :initial-element :forward)
  ;; todo doesn't capture how "turned" the enemy is. for now we'll
  ;; use the most turned sprite
  "which way the enemy's sprite should face, left, right, or forward. not applicable to all enemies.")
(defvar enm-health
  (make-array NUM-ENM :element-type 'float :initial-element 0.0)
  "enemy health values")
(defvar enm-control
  (make-array NUM-ENM :element-type '(or null function) :initial-element nil)
  "Control function guiding this enemy, called every frame. Can be a coroutine, but the coroutine must never exit, only yield. To exit, delete the enemy.")
(defvar enm-extras
  (let ((result (make-array NUM-ENM :element-type '(or null hash-table) :initial-element nil)))
	(dotimes (i NUM-ENM)
	  (setf (aref result i) (make-hash-table :size 4)))
	result)
  "Auxiliary hashtable of any data the enemy need to store. If something's very commonly used across many enemies, it should get its own array.")

(defun spawn-enemy (type x y health control-function)
  ;; todo: if linear scan for a free slot becomes a concern, can consider implementing a next-fit style pointer, or using a bit-vector
  (let ((id (position :none enm-types)))
	(unless id
	  (error "No more open enemy slots!"))
	(setf (aref enm-types id) type)
	(setf (aref enm-xs id) x)
	(setf (aref enm-ys id) y)
	(setf (aref enm-health id) health)
	(setf (aref enm-control id) control-function)
	id))

(defun delete-enemy (id)
  (when (eq :none (aref enm-types id))
	(error "deleting inactive enemy slot"))
  ;; no need to clean others as they'll be lazily filled by the next spawn call
  (setf (aref enm-types id) :none)
  (setf (aref enm-control id) nil)
  (clrhash (aref enm-extras id)))

(defun tick-enemies ()
  (loop for id from 0
		for type across enm-types
		do (when (not (eq :none type))
			 (funcall (aref enm-control id) id)
			 (when (<= (aref enm-health id) 0)
			   ;; todo sfx
			   (delete-enemy id)))))

(defun force-clear-bullet-and-enemy ()
  (fill enm-types :none)
  (fill enm-control nil)
  (fill bullet-types :none)
  (fill bullet-control nil))

(defun draw-enemies (textures)
  (loop
	for id from 0
	for type across enm-types
	for x across enm-xs
	for y across enm-ys
	for render-x = (+ x playfield-render-offset-x)
	for render-y = (+ y playfield-render-offset-y)
	do (case type
		 (:red-fairy
		  (draw-sprite textures :red-fairy render-x render-y :raywhite))
		 (:none t))))

;; 

(defcoroutine stationary-shoot-at-player (id)
  (loop
	(when (zerop (mod frames 25))
	  (let* ((pos (vec (aref enm-xs id) (aref enm-ys id)))
			 (player-pos (vec player-x player-y))
			 (dir (nvscale (nvunit (v- player-pos pos)) 3.0)))
		(spawn-bullet :pellet-white
					  (vx pos) (vy pos)
					  (vx dir) (vy dir)
					  0 'bullet-control-linear)
		(raylib:play-sound (sebundle-shoot0 sounds))))
	(yield)))

;; Boss management
(defvar current-boss-name nil)
(defvar current-spell-name nil)
(defvar current-boss-timer-frames 0.0) ;; will be converted to seconds for display

;; Stage sequencing

;; player
(defvar player-x 0)
(defvar player-y (- playfield-max-y 10.0))
(defvar player-speed 200)
(defvar player-xv 0.0)
(defvar player-yv 0.0)

(defun handle-input ()
  ;; level triggered stuff
  (when (raylib:is-key-down :key-left)
	(incf player-xv -1))
  (when (raylib:is-key-down :key-right)
	(incf player-xv 1))
  (when (raylib:is-key-down :key-up)
	(incf player-yv -1))
  (when (raylib:is-key-down :key-down)
	(incf player-yv 1))

  ;; edge triggered stuff
  (loop for k = (raylib:get-key-pressed) then (raylib:get-key-pressed)
		while (not (eq :key-null k))
		do (case k
			 (:key-z t) ;; todo shooting
			 (:key-x t) ;; todo (maybe) bombing
			 (:key-space
			  (spawn-enemy :red-fairy
						   player-x
						   (- player-y 10) 50 (make-coroutine 'stationary-shoot-at-player))
			  (spawn-bullet :pellet-white
							player-x
							(+ player-y 10) 0 -5 0 'bullet-control-linear)
			  (raylib:play-sound (sebundle-shoot0 sounds)))
			 (:key-y (force-clear-bullet-and-enemy)))))

(defun handle-player-movement ()
  (when (not (and (zerop player-xv) (zerop player-yv)))
    (let* ((delta-time (raylib:get-frame-time))
           (velocity (* player-speed delta-time (if (raylib:is-key-down :key-left-shift) 0.5 1)))
           (normalized-direction (vunit (vec player-xv player-yv)))
           (acceleration (v* normalized-direction velocity))
           (new-x (+ player-x (vx acceleration)))
           (new-y (+ player-y (vy acceleration))))
	  ;; todo: refine this so that you can bottomdrag instead of not moving at all
      (when (not (or (> new-x playfield-max-x) (> new-y playfield-max-y) (< new-y playfield-min-y) (< new-x playfield-min-x)))
        (setf player-x new-x)
        (setf player-y new-y)))
    (setf player-xv 0.0)
    (setf player-yv 0.0)))

(defvar focus-sigil-strength 0.0)
(defun draw-player (textures)
  (let* ((render-player-x (+ player-x playfield-render-offset-x))
		 (render-player-y (+ player-y playfield-render-offset-y)))
	;; player sprite (todo: directional moving sprites)
	(let ((x-texture-index (truncate (mod (/ frames 9) 8))))
	  (raylib:draw-texture-rec
	   (txbundle-reimu textures)
	   (raylib:make-rectangle :x (* 32 x-texture-index) :y 0 :width 32 :height 48)
	   (vec (- render-player-x 16) (- render-player-y 25))
	   :raywhite))
	
	;; focus sigil
	(if (raylib:is-key-down :key-left-shift)
		(when (< focus-sigil-strength 1.0)
		  (incf focus-sigil-strength 0.1))
		(when (> focus-sigil-strength 0.0)
		  (decf focus-sigil-strength 0.1)))
	(when (> focus-sigil-strength 0.0)
	  (rlgl:push-matrix)
	  (rlgl:translate-f render-player-x render-player-y 0.0) ;; move to where we are
	  (rlgl:rotate-f (mod frames 360.0) 0.0 0.0 1.0) ;; spin
	  (draw-sprite textures :focus-sigil
				   0.0 0.0 ;; manually translated to final position above
				   (raylib:make-rgba 255 255 255 (round (* 255 focus-sigil-strength))))
	  (rlgl:pop-matrix))))

(defstruct sebundle
  "Bundle of loaded sound effects"
  spellcapture spelldeclare
  longcharge shortcharge
  enmdie bossdie
  shoot0 shoot1 shoot2
  extend graze bell
  oldvwoopfast oldvwoopslow
  pause menuselect
  timeout timeoutwarn)
(defvar sounds nil)
(defun load-sfx ()
  (flet ((lsfx (file) (raylib:load-sound (concatenate 'string "assets/sfx/" file))))
	(setf sounds
		  (make-sebundle
		   :spellcapture (lsfx "se_cardget.wav")
		   :spelldeclare (lsfx "se_cat00.wav")
		   :longcharge (lsfx "se_ch00.wav")
		   :shortcharge (lsfx "se_ch02.wav")
		   :enmdie (lsfx "se_enep00.wav")
		   :bossdie (lsfx "se_enep01.wav")
		   :shoot0 (lsfx "se_tan00.wav")
		   :shoot1 (lsfx "se_tan01.wav")
		   :shoot2 (lsfx "se_tan02.wav")
		   :extend (lsfx "se_extend.wav")
		   :graze (lsfx "se_graze.wav")
		   :bell (lsfx "se_kira00.wav")
		   :oldvwoopfast (lsfx "se_power1.wav")
		   :oldvwoopslow (lsfx "se_power2.wav")
		   :pause (lsfx "se_pause.wav")
		   :menuselect (lsfx "se_select00.wav")
		   :timeout (lsfx "se_timeout.wav")
		   :timeoutwarn (lsfx "se_timeout2.wav")))))
(defun unload-sfx ()
  ;; meh, todo.
  )

(defun render-all (textures)
  (raylib:clear-background bgcolor)
  (draw-player textures)

  (draw-enemies textures)
  (draw-bullets textures)
  
  (raylib:draw-texture (txbundle-hud textures) 0 0 :raywhite)
  (raylib:draw-text (format nil "ENM: ~d" (- NUM-ENM (count :none enm-types)))
					550 400
					18 :raywhite)
  (raylib:draw-text (format nil "BLT: ~d" (- NUM-BULLETS (count :none bullet-types)))
					550 425
					18 :raywhite)
  (raylib:draw-fps 550 450))

(defvar ojamajo-carnival nil)
(defun load-audio ()
  (raylib:init-audio-device)
  (setf ojamajo-carnival (raylib:load-music-stream "assets/bgm/ojamajo_carnival.wav")))
(defun unload-audio ()
  (raylib:stop-music-stream ojamajo-carnival)
  (raylib:unload-music-stream ojamajo-carnival))

(defun reset-to (frame)
  "Resets frame counter and music playback to specific frame.
For use in interactive development."
  (force-clear-bullet-and-enemy)
  (setf frames frame)
  (raylib:seek-music-stream ojamajo-carnival (/ frame 60.0)))

(defun main ()
  ;; Starts a REPL, connect with slime-connect in emacs
  (swank:create-server)
  (raylib:with-window
	  (640 480 "thdawn")
	(raylib:set-target-fps 60)
	(raylib:set-exit-key 0)
	(load-audio)
	(load-sfx)
	(let ((textures (load-textures)))
	  (raylib:play-music-stream ojamajo-carnival)
	  (loop
		(when (raylib:window-should-close)
		  (return))
		(raylib:update-music-stream ojamajo-carnival)
		(handle-input)
		(handle-player-movement)
		(tick-bullets)
		(tick-enemies)
		(raylib:with-drawing
		  (render-all textures))
		(incf frames))
	  (unload-audio)
	  (unload-textures textures)
	  (unload-sfx))))
