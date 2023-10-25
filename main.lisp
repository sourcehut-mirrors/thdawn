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
(defconstant playfield-max-x (- 416 31))
(defconstant playfield-max-y (- 463 15))

(defun despawn-out-of-bound-bullet (id)
  (let ((type (aref bullet-types id))
		(x (aref bullet-xs id))
		(y (aref bullet-ys id)))
	;; tolerate this, we could be calling this after a bullet's control function kills
	;; itself
	(when (and (not (eq :none type))
			   (or (> x playfield-max-x) ;; todo: fuzz factor
				   (< x -5) ;;  todo: less hardcoding
				   (< y -5)
				   (> y playfield-max-y)))
	  (delete-bullet id))))

(defun tick-bullets ()
  (loop for id from 0
		for type across bullet-types
		do (when (not (eq :none type))
			 (funcall (aref bullet-control id) id)
			 (despawn-out-of-bound-bullet id))))

(defun draw-bullets (textures)
  (loop for id from 0
    for type across bullet-types
    do (when (not (eq :none type))
         (draw-texture-rec (txbundle-bullet2 textures) (make-rectangle :x 96 :y 0 :width 16 :height 16) (make-vector2 :x (aref bullet-xs id) :y (aref bullet-ys id)) :raywhite))))

(defconstant NUM-ENM 256)

;; Boss management

;; Stage sequencing

;; player
(defvar player-x 0)
(defvar player-y 0)

(defun handle-input ()
  ;; todo: make this less awful (diagonal normalization, clamping at boundaries, proper velocity, etc.)
  (when (is-key-down :key-left)
	(incf player-x -3))
  (when (is-key-down :key-right)
	(incf player-x 3))
  (when (is-key-down :key-up)
	(incf player-y -3))
  (when (is-key-down :key-down)
	(incf player-y 3))
  (when (is-key-down :key-space)
  (spawn-bullet 0 player-x (+ player-y 10) 0 5 0 :bullet-control-linear))
  )

(defstruct txbundle
  "Bundle of loaded texture objects, because using globals causes segfaults somehow"
  hud
  bullet2)
(defun load-textures ()
  (make-txbundle
   :hud (load-texture "ryannlib_v1.02/data_assets/THlib/UI/ui_bg.png")
   :bullet2 (load-texture "ryannlib_v1.02/data_assets/THlib/bullet/bullet2.png")))
(defun unload-textures (textures)
  (unload-texture (txbundle-hud textures))
  (unload-texture (txbundle-bullet2 textures)))

(defun render-all (textures)
  (clear-background bgcolor)
  (draw-circle (+ player-x 31) (+ player-y 15) 8.0 :raywhite)
  (draw-texture (txbundle-hud textures) 0 0 :raywhite)
  (draw-bullets textures)
  (draw-fps 10 10))

(defvar ojamajo-carnival nil)
(defun load-audio ()
  (init-audio-device)
  (setf ojamajo-carnival (load-music-stream "bgm/ojamajo_carnival.wav")))
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
	(let ((textures (load-textures)))
	  (play-music-stream ojamajo-carnival)
	  (loop
		(when (window-should-close)
		  (return))
		(update-music-stream ojamajo-carnival)
		(handle-input)
		(with-drawing
		  (render-all textures))
		(incf frames))
	  (unload-audio)
	  (unload-textures textures)
	  )))
