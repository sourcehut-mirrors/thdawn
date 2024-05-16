;; All symbols declared from now on are in the thdawn namespace
(in-package :thdawn)

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

(defvar frames 0 "Number of frames the current stage has been running")
(defvar show-hitboxes nil)
(defvar graze 0)
(defvar paused nil)
(defvar current-boss-name nil)
(defvar current-spell-name nil)
(defvar current-boss-timer-frames 0.0) ;; will be converted to seconds for display
(defparameter graze-radius 22.0)
(defparameter hit-radius 3.0)
(defvar player-x 0)
(defvar player-y (- playfield-max-y 10.0))
(defvar player-speed 200)
(defvar player-xv 0.0)
(defvar player-yv 0.0)

(defvar ojamajo-carnival nil)
(defun load-audio ()
  (raylib:init-audio-device)
  (setf ojamajo-carnival (raylib:load-music-stream "assets/bgm/ojamajo_carnival.wav")))
(defun unload-audio ()
  (raylib:stop-music-stream ojamajo-carnival)
  (raylib:unload-music-stream ojamajo-carnival))

;; Try changing me, then updating the game live in the REPL!
(defparameter bgcolor :black)

;; Bullet system
(defconstant NUM-BULLETS 512)
(defstruct bullet
  (type :none :type keyword)
  (color :white :type keyword)
  (x 0.0 :type float)
  (y 0.0 :type float)
  (facing 0.0 :type float) ;; radians
  (speed 0.0 :type float)
  (grazed nil :type boolean)
  (control (lambda (_bullet))) ;; todo: figure out why :type function blows up here
  ;; consider removing or not allocating by default
  (extras (make-hash-table :size 4) :type hash-table))

(defvar live-bullets
  (make-array NUM-BULLETS :element-type '(or null bullet) :initial-element nil))

;; Common bullet control functions
(defun bullet-control-linear (bullet)
  "A simple control function that advances the bullet's position according to its (fixed) facing and speed"
  (let ((facing (bullet-facing bullet))
		(speed (bullet-speed bullet)))
	(incf (bullet-x bullet) (* speed (cos facing)))
	(incf (bullet-y bullet) (* speed (sin facing)))))

;; TODO: Others common control functions we might want (acceleration, deceleration, etc.)

(defun spawn-bullet (type x y facing speed control-function)
  (let ((idx (position nil live-bullets)))
	(unless idx
	  (error "No more open bullet slots! Either bullets are not being killed properly or the pattern is too complex"))
	(setf (aref live-bullets idx)
		  (make-bullet :type type
					   :x x :y y
					   :facing facing
					   :speed speed
					   :control control-function))))

(defun delete-bullet (bullet)
  (let ((idx (position bullet live-bullets)))
	(when idx
	  ;; tolerate killing already-removed/dead bullets
	  (setf (aref live-bullets idx) nil))))

(defun despawn-out-of-bound-bullet (bullet)
  (let ((x (bullet-x bullet))
		(y (bullet-y bullet)))
	(when (or (> x (+ playfield-max-x oob-bullet-despawn-fuzz))
			  (< x (- playfield-min-x oob-bullet-despawn-fuzz))
			  (< y (- playfield-min-y oob-bullet-despawn-fuzz))
			  (> y (+ playfield-max-y oob-bullet-despawn-fuzz)))
	  (delete-bullet bullet))))

(defun tick-bullets ()
  (loop for bullet across live-bullets
		when bullet
		do (funcall (bullet-control bullet) bullet)
		   (despawn-out-of-bound-bullet bullet)))

(defun bullet-hit-radius (type)
  (case type
	(:pellet-white 2.0)
	(t 0.0)))

(defun draw-bullets (textures)
  (loop
	for bullet across live-bullets
	when bullet
	  do
		 (let ((render-x (+ (bullet-x bullet) playfield-render-offset-x))
			   (render-y (+ (bullet-y bullet) playfield-render-offset-y))
			   (type (bullet-type bullet)))
			   (case type
				 (:pellet-white
				  (draw-sprite textures :pellet-white render-x render-y :raywhite))
				 (:none t))
			   (when show-hitboxes
				 (raylib:draw-circle-v (vec render-x render-y) (bullet-hit-radius type) :red)))))

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

(defun process-collisions ()
  (loop for bullet across live-bullets
		if bullet
		  do (when (and (not (bullet-grazed bullet))
						(raylib:check-collision-circles
						 (vec player-x player-y) graze-radius
						 (vec (bullet-x bullet) (bullet-y bullet)) (bullet-hit-radius (bullet-type bullet))))
			   (incf graze)
			   (setf (bullet-grazed bullet) t)
			   (raylib:play-sound (sebundle-graze sounds))) ;; todo make this sound better?
			 (when (raylib:check-collision-circles
					(vec player-x player-y) hit-radius
					(vec (bullet-x bullet) (bullet-y bullet)) (bullet-hit-radius (bullet-type bullet))) ;; todo hitbox size per bullet type
			   (raylib:play-sound (sebundle-playerdie sounds)))))

(defun force-clear-bullet-and-enemy ()
  (setf graze 0)
  (fill enm-types :none)
  (fill enm-control nil)
  (fill live-bullets nil))

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

(defun shoot-at-player (srcpos)
  (let* ((player-pos (vec player-x player-y))
		 (diff (v- player-pos srcpos))
		 (facing (atan (vy diff) (vx diff))))
	(spawn-bullet :pellet-white
				  (vx srcpos) (vy srcpos)
				  facing 3.0
				  'bullet-control-linear)
	(raylib:play-sound (sebundle-shoot0 sounds))))

(defcoroutine stationary-shoot-at-player (id)
  (loop
	(when (zerop (mod frames 50))
	  (shoot-at-player (vec (aref enm-xs id) (aref enm-ys id))))
	(yield)))

(defcoroutine wiggle-shoot-at-player (id)
  (let ((dir -1)
		(dir-frames 0))
	(loop
	  (incf (aref enm-xs id) dir)
	  (when (zerop (mod frames 50))
		(shoot-at-player (vec (aref enm-xs id) (aref enm-ys id))))
	  (incf dir-frames)
	  (when (= dir-frames 180)
		(setf dir (* -1 dir))
		(setf dir-frames 0))
	  (yield))))

(defun handle-input ()
  ;; level triggered stuff
  (unless paused
	(when (raylib:is-key-down :key-left)
	  (incf player-xv -1))
	(when (raylib:is-key-down :key-right)
	  (incf player-xv 1))
	(when (raylib:is-key-down :key-up)
	  (incf player-yv -1))
	(when (raylib:is-key-down :key-down)
	  (incf player-yv 1)))

  ;; edge triggered stuff
  (loop for k = (raylib:get-key-pressed) then (raylib:get-key-pressed)
		while (not (eq :key-null k))
		do (case k
			 (:key-z t) ;; todo shooting
			 (:key-x t) ;; todo (maybe) bombing
			 (:key-f3 (setf show-hitboxes (not show-hitboxes)))
			 (:key-escape
			  (setf paused (not paused))
			  (if paused
				  (progn
					(raylib:play-sound (sebundle-pause sounds))
					(raylib:pause-music-stream ojamajo-carnival))
				  (progn
					(raylib:resume-music-stream ojamajo-carnival))))
			 (:key-space
			  (spawn-enemy :red-fairy
						   player-x
						   (- player-y 10) 50 (make-coroutine 'wiggle-shoot-at-player)))
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
	   (vec (- render-player-x 16) (- render-player-y 24))
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
	  (rlgl:pop-matrix))

	(when show-hitboxes
	  (raylib:draw-circle-v (vec render-player-x render-player-y) graze-radius :green)
	  (raylib:draw-circle-v (vec render-player-x render-player-y) hit-radius :red))))

(defun render-all (textures)
  (raylib:clear-background bgcolor)
  (draw-player textures)

  (draw-enemies textures)
  (draw-bullets textures)

  (when paused
	(raylib:draw-text "PAUSED" 175 150 28 :purple))
  
  (raylib:draw-texture (txbundle-hud textures) 0 0 :raywhite)
  (raylib:draw-text (format nil "GRAZE: ~d" graze)
					500 375
					18 :raywhite)
  (raylib:draw-text (format nil "ENM: ~d" (- NUM-ENM (count :none enm-types)))
					500 400
					18 :raywhite)
  (raylib:draw-text (format nil "BLT: ~d" (- NUM-BULLETS (count nil live-bullets)))
					500 425
					18 :raywhite)
  (raylib:draw-fps 500 450))

(defun reset-to (frame)
  "Resets frame counter and music playback to specific frame.
For use in interactive development."
  (force-clear-bullet-and-enemy)
  (setf frames frame)
  (raylib:seek-music-stream ojamajo-carnival (/ frame 60.0)))

(defun main ()
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
		(handle-input)
		(raylib:update-music-stream ojamajo-carnival)
		(unless paused
		  (handle-player-movement)
		  (tick-bullets)
		  (tick-enemies)
		  (process-collisions))
		(raylib:with-drawing
		  (render-all textures))
		(unless paused (incf frames)))
	  (unload-audio)
	  (unload-textures textures)
	  (unload-sfx))))
