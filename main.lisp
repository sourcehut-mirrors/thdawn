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
;; idea is to have logical game stuff stored where 0 0 is the top left of the hud texture, and we just offset everything by +31, +15 to render.
(defconstant playfield-render-offset-x 31)
(defconstant playfield-render-offset-y 15)
(defconstant playfield-min-x 0)
(defconstant playfield-min-y 0)
(defconstant playfield-max-x 385)
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
		  (draw-texture-rec
		   (txbundle-bullet2 textures)
		   (make-rectangle :x 176 :y 224 :width 16 :height 16)
		   (vec (- render-x 8)
				(- render-y 8)) ;; center texture onto the position
		   :raywhite))
		 (:none t))))

(defconstant NUM-ENM 256)

;; Boss management

;; Stage sequencing

;; player
(defvar player-x (/ (- playfield-max-x playfield-min-x) 2))
(defvar player-y (- playfield-max-y 10))
(defvar player-speed 200)
(defvar player-xv 0.0)
(defvar player-yv 0.0)

(defun handle-input ()
  ;; todo: make this less awful (diagonal normalization, proper velocity, etc.)
  ;; level triggered stuff
  (when (is-key-down :key-left)
	  (incf player-xv -1))
	(when (is-key-down :key-right)
	  (incf player-xv 1))
	(when (is-key-down :key-up)
	  (incf player-yv -1))
	(when (is-key-down :key-down)
	  (incf player-yv 1))

  ;; edge triggered stuff
  (loop for k = (get-key-pressed) then (get-key-pressed)
		while (not (eq :key-null k))
		do (case k
			 (:key-space
			  (spawn-bullet :pellet-white
							player-x
							(+ player-y 10) 0 -5 0 'bullet-control-linear)
			  (play-sound (sebundle-shoot0 sounds))
			  ))))

(defun handle-player-movement ()
  (when (not (and (eq player-xv 0.0) (eq player-yv 0.0)))
    (let* ((delta-time (get-frame-time))
           (velocity (* player-speed delta-time))
           (normalized-direction (vunit (vec player-xv player-yv)))
           (acceleration (v* normalized-direction velocity))
           (new-vx (vx acceleration))
           (new-vy (vy acceleration))
           (new-x (+ player-x (truncate new-vx)))
           (new-y (+ player-y (truncate new-vy))))
      (when (not (or (> new-x playfield-max-x) (> new-y playfield-max-y) (< new-y -5) (< new-x -5)))
        (setf player-x new-x)
        (setf player-y new-y))
      delta-time)
    (setf player-xv 0.0)
    (setf player-yv 0.0)
    )
  )

(defstruct txbundle
  "Bundle of loaded texture objects, because using globals causes segfaults somehow"
  hud
  bullet2)
(defun load-textures ()
  (make-txbundle
   :hud (load-texture "assets/img/ui_bg.png")
   :bullet2 (load-texture "assets/img/bullet2.png")))
(defun unload-textures (textures)
  (unload-texture (txbundle-hud textures))
  (unload-texture (txbundle-bullet2 textures)))

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
  (flet ((lsfx (file) (load-sound (concatenate 'string "assets/sfx/" file))))
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
  (clear-background bgcolor)
  (draw-circle (round (+ player-x playfield-render-offset-x))
			   (round (+ player-y playfield-render-offset-y)) 8.0 :raywhite)
  (draw-bullets textures)
  (draw-texture (txbundle-hud textures) 0 0 :raywhite)
  (draw-text (format nil "BLT: ~d" (- NUM-BULLETS (count :none bullet-types)))
			 550 425
			 18 :raywhite)
  (draw-fps 550 450))

(defvar ojamajo-carnival nil)
(defun load-audio ()
  (init-audio-device)
  (setf ojamajo-carnival (load-music-stream "assets/bgm/ojamajo_carnival.wav")))
(defun unload-audio ()
  (stop-music-stream ojamajo-carnival)
  (unload-music-stream ojamajo-carnival))

(defun reset-to (frame)
  "Resets frame counter and music playback to specific frame.
For use in interactive development."
  ;; todo also kill all enemies

  (fill bullet-types :none)
  (fill bullet-colors :white)
  (fill bullet-xs 0.0)
  (fill bullet-ys 0.0)
  (fill bullet-xvs 0.0)
  (fill bullet-yvs 0.0)
  (fill bullet-control nil)
  (fill bullet-facing 0.0)
  (setf frames frame)
  (seek-music-stream ojamajo-carnival (/ frame 60.0)))

(defun main ()
  ;; Starts a REPL, connect with slime-connect in emacs
  (swank:create-server)
  (with-window
	  (640 480 "thdawn")
	(set-target-fps 60)
	(set-exit-key 0)
	(load-audio)
	(load-sfx)
	(let ((textures (load-textures)))
	  (play-music-stream ojamajo-carnival)
	  (loop
		(when (window-should-close)
		  (return))
		(update-music-stream ojamajo-carnival)
		(handle-input)
    (handle-player-movement)
		(tick-bullets)
		(with-drawing
		  (render-all textures))
		(incf frames))
	  (unload-audio)
	  (unload-textures textures)
	  (unload-sfx))))
