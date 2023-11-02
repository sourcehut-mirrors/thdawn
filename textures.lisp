(in-package :thdawn)

(defstruct txbundle
  "Bundle of loaded texture objects"
  reimu
  enemy1
  hud
  misc
  bullet2)
(defun load-textures ()
  (make-txbundle
   :reimu (raylib:load-texture "assets/img/reimu.png")
   :enemy1 (raylib:load-texture "assets/img/enemy1.png")
   :hud (raylib:load-texture "assets/img/ui_bg.png")
   :misc (raylib:load-texture "assets/img/misc.png")
   :bullet2 (raylib:load-texture "assets/img/bullet2.png")))
(defun unload-textures (textures)
  (raylib:unload-texture (txbundle-reimu textures))
  (raylib:unload-texture (txbundle-hud textures))
  (raylib:unload-texture (txbundle-bullet2 textures))
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
		   :center-shift (vec -8 -8)))

	;; enemies
	(setf (gethash :red-fairy ret)
		  (make-sprite-descriptor
		   :tx-accessor #'txbundle-enemy1
		   :bounds (raylib:make-rectangle :x 0 :y 384 :width 32 :height 32)
		   :center-shift (vec -16.0 -16.0)))

	;; misc
	(setf (gethash :focus-sigil ret)
		  (make-sprite-descriptor
		   :tx-accessor #'txbundle-misc
		   :bounds (raylib:make-rectangle :x 128 :y 0 :width 64 :height 64)
		   :center-shift (vec -32.0 -32.0)))
	ret))

(defun draw-sprite (textures sprite-id x y color)
  (let ((data (gethash sprite-id sprite-data)))
	(raylib:draw-texture-rec
	 (funcall (sprite-descriptor-tx-accessor data) textures)
	 (sprite-descriptor-bounds data)
	 (nv+ (vec x y) (sprite-descriptor-center-shift data))
	 color)))
