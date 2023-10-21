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

;; Bullet and enemy system

;; Boss management

;; Stage sequencing

;; sfx and music

;; resource loading

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
	(let ((hud-texture (load-texture "ryannlib_v1.02/data_assets/THlib/UI/ui_bg.png")))
	  (loop
		(if (window-should-close)
			(return))
		(with-drawing
		  (clear-background bgcolor)
		  (draw-texture hud-texture 0 0 :raywhite)
		  (draw-fps 10 10))))))

(main)
