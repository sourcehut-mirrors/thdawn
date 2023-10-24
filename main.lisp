;; These arrange for the dependency trees of these libraries to be loaded into the process from disk.
;; In a "real project" and when we need to do a release for real, this would be declared in an ASDF system, and not manually done here.
(require :cl-raylib)
(require :swank)
(require :cl-coroutine)
(require :cffi)

;; Declares current namespace and brings all `use`d bindings into
;; (unqualified) scope within it
(defpackage :thdawn
  (:use :cl :raylib :cl-coroutine))

;; All symbols declared from now on are in the thdawn namespace
(in-package :thdawn)

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
  (make-array NUM-BULLETS :element-type 'float :initial-element 0)
  "bullet x positions")
(defvar bullet-ys
  (make-array NUM-BULLETS :element-type 'float :initial-element 0)
  "bullet y positions")
(defvar bullet-xvs
  (make-array NUM-BULLETS :element-type 'float :initial-element 0)
  "bullet x velocities")
(defvar bullet-yvs
  (make-array NUM-BULLETS :element-type 'float :initial-element 0)
  "bullet y velocities")
(defvar bullet-control
  (make-array NUM-BULLETS :element-type '(or null function) :initial-element NIL)
  "Control function guiding this bullet's movement, called every frame.")
(defvar bullet-extras
  (let ((result (make-array NUM-BULLETS)))
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

(defconstant NUM-ENM 256)

;; Boss management

;; Stage sequencing

;; player
(defvar player-x 0)
(defvar player-y 0)

;; [31-416] x bounds of playfield in the hud texture
;; [15-463] y bounds of playfield in the hud texture
;; idea is to have logical game stuff stored where 0 0 is the top left of the hud texture, and we just offset everything by +31, +15 to render.

(defun handle-input ()
  ;; todo: make this less awful (diagonal normalization, clamping at boundaries, proper velocity, etc.)
  (when (is-key-down :key-left)
	(incf player-x -3))
  (when (is-key-down :key-right)
	(incf player-x 3))
  (when (is-key-down :key-up)
	(incf player-y -3))
  (when (is-key-down :key-down)
	(incf player-y 3)))

(defun render-all (hud-texture)
  (clear-background bgcolor)
  (draw-circle (+ player-x 31) (+ player-y 15) 8.0 :raywhite)
  (draw-texture hud-texture 0 0 :raywhite)
  (draw-fps 10 10))

(defvar ojamajo-carnival nil)
(defun load-audio ()
  (init-audio-device)
  (setf ojamajo-carnival (load-music-stream "bgm/ojamajo_carnival.wav")))
(defun unload-audio ()
  (stop-music-stream ojamajo-carnival)
  (unload-music-stream ojamajo-carnival))

(defvar frames 0 "Number of frames the current stage has been running")

(defun reset-to (frame)
  "Kills all enemies, resets frame counter and music to specific frame.
For use in interactive development."
  (setf frames frame)
  ;; todo not wrapped yet (seek-music-stream ojamajo-carnival (/ frame 60.0))
  )

(defun main ()
  ;; Starts a REPL, connect with slime-connect in emacs
  (swank:create-server)
  (with-window
	  (640 480 "thdawn")
	(set-target-fps 60)
	(set-exit-key 0)
	(load-audio)
	;; this is in a local variable because using file scope causes segfaults
	;; in load-texture. Idk why.
	(let ((hud-texture (load-texture "ryannlib_v1.02/data_assets/THlib/UI/ui_bg.png")))
	  (play-music-stream ojamajo-carnival)
	  (loop
		(when (window-should-close)
		  (return))
		(update-music-stream ojamajo-carnival)
		(handle-input)
		(with-drawing
		  (render-all hud-texture))
		(incf frames))
	  (unload-audio)
	  (unload-texture hud-texture))))

(main)
