(in-package :thdawn)

(defstruct txbundle
  "Bundle of loaded texture objects"
  reimu
  enemy1
  hud
  misc
  bullet1 bullet2 bullet3 bullet4 bullet5 bullet6)
(defun load-textures ()
  (flet ((ltex (file) (raylib:load-texture
					   (concatenate 'string "assets/img/" file))))
	(make-txbundle
	 :reimu (ltex "reimu.png")
	 :enemy1 (ltex "enemy1.png")
	 :hud (ltex "ui_bg.png")
	 :misc (ltex "misc.png")
	 :bullet1 (ltex "bullet1.png")
	 :bullet2 (ltex "bullet2.png")
	 :bullet3 (ltex "bullet3.png")
	 :bullet4 (ltex "bullet4.png")
	 :bullet5 (ltex "bullet5.png")
	 :bullet6 (ltex "bullet6.png"))))
(defun unload-textures (textures)
  (raylib:unload-texture (txbundle-reimu textures))
  (raylib:unload-texture (txbundle-hud textures))
  (raylib:unload-texture (txbundle-bullet1 textures))
  (raylib:unload-texture (txbundle-bullet2 textures))
  (raylib:unload-texture (txbundle-bullet3 textures))
  (raylib:unload-texture (txbundle-bullet4 textures))
  (raylib:unload-texture (txbundle-bullet5 textures))
  (raylib:unload-texture (txbundle-bullet6 textures))
  (raylib:unload-texture (txbundle-enemy1 textures))
  (raylib:unload-texture (txbundle-misc textures)))

(defstruct sprite-descriptor
  ;; txbundle-* accessor function for the texture this sprite is located on
  tx-accessor 
  ;; rectangle; x, y, width, and height of this sprite on its texture
  bounds 
  ;; vec2; how much to shift so that rendering is centered. Usually -width/2 or -height/2,
  ;; unless the sprite's semantic "center" is not its geometric center
  center-shift)

(defun make-sprite-data ()
  (let ((ret (make-hash-table))
		(shift8 (vec2 -8 -8))
		(shift16 (vec2 -16 -16)))
	(flet ((make (type accessor x y width height shift)
			 (setf (gethash type ret)
				   (make-sprite-descriptor
					:tx-accessor accessor
					:bounds (raylib:make-rectangle :x x :y y :width width :height height)
					:center-shift shift))))
	  ;; bullets
	  (make :pellet-red #'txbundle-bullet2 176 0 16 16 shift8)
	  (make :pellet-magenta #'txbundle-bullet2 176 32 16 16 shift8)
	  (make :pellet-blue #'txbundle-bullet2 176 80 16 16 shift8)
	  (make :pellet-cyan #'txbundle-bullet2 176 96 16 16 shift8)
	  (make :pellet-green #'txbundle-bullet2 176 112 16 16 shift8)
	  (make :pellet-yellow #'txbundle-bullet2 176 160 16 16 shift8)
	  (make :pellet-orange #'txbundle-bullet2 176 192 16 16 shift8)
	  (make :pellet-gray #'txbundle-bullet2 176 208 16 16 shift8)
	  (make :pellet-white #'txbundle-bullet2 176 224 16 16 shift8)
	  (make :small-star-red #'txbundle-bullet2 96 0 16 16 shift8)
	  (make :small-star-magenta #'txbundle-bullet2 96 32 16 16 shift8)
	  (make :small-star-blue #'txbundle-bullet2 96 64 16 16 shift8)
	  (make :small-star-cyan #'txbundle-bullet2 96 96 16 16 shift8)
	  (make :small-star-green #'txbundle-bullet2 96 128 16 16 shift8)
	  (make :small-star-yellow #'txbundle-bullet2 96 160 16 16 shift8)
	  (make :small-star-orange #'txbundle-bullet2 96 192 16 16 shift8)
	  (make :small-star-white #'txbundle-bullet2 96 224 16 16 shift8)
	  (make :small-star-black #'txbundle-bullet2 96 240 16 16 shift8)
	  (make :big-star-red #'txbundle-bullet2 224 0 32 32 shift16)
	  (make :big-star-magenta #'txbundle-bullet2 224 32 32 32 shift16)
	  (make :big-star-blue #'txbundle-bullet2 224 64 32 32 shift16)
	  (make :big-star-cyan #'txbundle-bullet2 224 96 32 32 shift16)
	  (make :big-star-green #'txbundle-bullet2 224 128 32 32 shift16)
	  (make :big-star-yellow #'txbundle-bullet2 224 160 32 32 shift16)
	  (make :big-star-orange #'txbundle-bullet2 224 192 32 32 shift16)
	  (make :big-star-white #'txbundle-bullet2 224 224 32 32 shift16)

	  ;; enemies
	  (make :red-fairy #'txbundle-enemy1 0 384 32 32 shift16)

	  ;; misc
	  (make :focus-sigil #'txbundle-misc 128 0 64 64 (vec2 -32.0 -32.0))
	  (make :mainshot #'txbundle-reimu 192 160 64 16 (vec2 -54.0 -8.0))
	  ret)))
(defparameter sprite-data (make-sprite-data))

(defun draw-sprite (textures sprite-id x y color)
  (let ((data (gethash sprite-id sprite-data)))
	(raylib:draw-texture-rec
	 (funcall (sprite-descriptor-tx-accessor data) textures)
	 (sprite-descriptor-bounds data)
	 (nv+ (vec x y) (sprite-descriptor-center-shift data))
	 color)))

(let ((zero (vec2 0.0 0.0)))
  (defun draw-sprite-with-rotation (textures sprite-id rotation x y color)
	(let* ((data (gethash sprite-id sprite-data))
		   (center-shift (sprite-descriptor-center-shift data)))
	  (rlgl:push-matrix)
	  (rlgl:translate-f x y 0.0)
	  (rlgl:rotate-f rotation 0.0 0.0 1.0)
	  (rlgl:translate-f (+ (vx2 center-shift)) (+ (vy2 center-shift)) 0.0)
	  (raylib:draw-texture-rec
	   (funcall (sprite-descriptor-tx-accessor data) textures)
	   (sprite-descriptor-bounds data)
	   zero
	   color)
	  (rlgl:pop-matrix))))
