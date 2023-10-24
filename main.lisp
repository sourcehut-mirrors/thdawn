;; These arrange for the dependency trees of these libraries to be loaded into the process from disk.
;; In a "real project" and when we need to do a release for real, this would be declared in an ASDF system, and not manually done here.
(require :cl-raylib)
(require :swank)
(require :cl-coroutine)

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

(defcoroutine test-coro (param)
  (yield)
  ;; do some bullet spawning logic
  (yield))

(defun main ()
  ;; Starts a REPL, connect with slime-connect in emacs
  (swank:create-server)
  (let ((coro-test (make-coroutine 'test-coro)))
	(funcall coro-test "param")
	(funcall coro-test "param")
	(funcall coro-test "param"))
  (with-window
	  (640 480 "thdawn")
	(set-target-fps 60)
	(set-exit-key 0)
	(init-audio-device)
	;; these are in local variables because putting them at file scope causes segfaults
	;; in load-texture. Idk why.
	(let ((ojamajo-carnival (load-music-stream "bgm/ojamajo_carnival.wav"))
		  (hud-texture (load-texture "ryannlib_v1.02/data_assets/THlib/UI/ui_bg.png")))
	  (play-music-stream ojamajo-carnival)
	  (loop
		(when (window-should-close)
		  (return))
		(update-music-stream ojamajo-carnival)
		(with-drawing
			(clear-background bgcolor)
		  (draw-texture hud-texture 0 0 :raywhite)
		  (draw-fps 10 10)))
	  (stop-music-stream ojamajo-carnival)
	  (unload-music-stream ojamajo-carnival)
	  (unload-texture hud-texture))))

(main)
