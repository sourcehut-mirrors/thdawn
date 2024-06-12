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

(defvar sprite-data
  (let ((ret (make-hash-table)))
	;; bullets
	(setf (gethash :pellet-white ret)
		  (make-sprite-descriptor
		   :tx-accessor #'txbundle-bullet2
		   :bounds (raylib:make-rectangle :x 176 :y 224 :width 16 :height 16)
		   :center-shift (vec2 -8 -8)))

	;; enemies
	(setf (gethash :red-fairy ret)
		  (make-sprite-descriptor
		   :tx-accessor #'txbundle-enemy1
		   :bounds (raylib:make-rectangle :x 0 :y 384 :width 32 :height 32)
		   :center-shift (vec2 -16.0 -16.0)))

	;; misc
	(setf (gethash :focus-sigil ret)
		  (make-sprite-descriptor
		   :tx-accessor #'txbundle-misc
		   :bounds (raylib:make-rectangle :x 128 :y 0 :width 64 :height 64)
		   :center-shift (vec2 -32.0 -32.0)))

	(setf (gethash :mainshot ret)
		  (make-sprite-descriptor
		   :tx-accessor #'txbundle-reimu
		   :bounds (raylib:make-rectangle :x 192 :y 160 :width 64 :height 16)
		   :center-shift (vec2 54.0 8.0)))
	ret))

(defun draw-sprite (textures sprite-id x y color)
  (let ((data (gethash sprite-id sprite-data)))
	(raylib:draw-texture-rec
	 (funcall (sprite-descriptor-tx-accessor data) textures)
	 (sprite-descriptor-bounds data)
	 (nv+ (vec x y) (sprite-descriptor-center-shift data))
	 color)))

(defun draw-sprite-with-rotation (textures sprite-id rotation x y color)
  (let* ((data (gethash sprite-id sprite-data))
		 (center-shift (sprite-descriptor-center-shift data)))
	(rlgl:push-matrix)
	(rlgl:translate-f x y 0.0)
	(rlgl:rotate-f rotation 0.0 0.0 1.0)
	(rlgl:translate-f (- (vx2 center-shift)) (- (vy2 center-shift)) 0.0)
	(raylib:draw-texture-rec
	 (funcall (sprite-descriptor-tx-accessor data) textures)
	 (sprite-descriptor-bounds data)
	 (vec2 0.0 0.0)
	 color)
	(rlgl:pop-matrix)))
