;; All symbols declared from now on are in the thdawn namespace
(in-package :thdawn)

;; [31-416] x bounds of playfield in the hud texture
;; [15-463] y bounds of playfield in the hud texture
;; idea is to have logical game x in [-192, 192] (192 is (416-31)/2, roughly), and y in [0, 448] 
;; logical game 0, 0 is at gl (31+(416-31)/2), 15
;; offset logical coords by 223, 15 to get to GL coords for render render
(defconstant +playfield-render-offset-x+ 223)
(defconstant +playfield-render-offset-y+ 15)
(defconstant +playfield-min-x+ -192)
(defconstant +playfield-min-y+ 0)
(defconstant +playfield-max-x+ 192)
(defconstant +playfield-max-y+ 448)
(defconstant +oob-bullet-despawn-fuzz+ 10)

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
(defvar player-y (- +playfield-max-y+ 10.0))
(defvar player-speed 200)
(defvar player-xv 0.0)
(defvar player-yv 0.0)

(defun clamp (v lower upper)
  (max (min v upper) lower))

(defvar ojamajo-carnival nil)
(defun load-audio ()
  (raylib:init-audio-device)
  (setf ojamajo-carnival (raylib:load-music-stream "assets/bgm/ojamajo_carnival.wav")))
(defun unload-audio ()
  (raylib:stop-music-stream ojamajo-carnival)
  (raylib:unload-music-stream ojamajo-carnival)
  (setf ojamajo-carnival nil))

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
	(when (or (> x (+ +playfield-max-x+ +oob-bullet-despawn-fuzz+))
			  (< x (- +playfield-min-x+ +oob-bullet-despawn-fuzz+))
			  (< y (- +playfield-min-y+ +oob-bullet-despawn-fuzz+))
			  (> y (+ +playfield-max-y+ +oob-bullet-despawn-fuzz+)))
	  (delete-bullet bullet))))

(defun tick-bullets ()
  (loop for bullet across live-bullets
		when bullet
		  do (funcall (bullet-control bullet) bullet)
			 (despawn-out-of-bound-bullet bullet)))

(defun bullet-family (type)
  (case type
	((:pellet-white :pellet-gray :pellet-orange :pellet-yellow
	  :pellet-green :pellet-cyan :pellet-blue :pellet-magenta :pellet-red)
	 :pellet)
	((:small-star-red :small-star-magenta :small-star-blue :small-star-cyan
	  :small-star-green :small-star-yellow :small-star-orange
	  :small-star-white :small-star-black)
	 :small-star)))

;; sbcl overeagerly optimizes the function type of this function
;; to be (or (single-float 2.0) (single-float 3.7) ...), which causes
;; problems for callers when live reloading to add a new value.
;; Explicitly declare a broader type definition to suppress that.
(declaim (ftype (function (keyword) single-float) bullet-hit-radius))
(defun bullet-hit-radius (type)
  (case (bullet-family type)
	((:pellet) 2.0)
	((:small-star) 3.7)
	(t 0.0)))

(defun draw-bullets (textures)
  (loop
	for bullet across live-bullets
	when bullet
	  do
		 (let ((render-x (+ (bullet-x bullet) +playfield-render-offset-x+))
			   (render-y (+ (bullet-y bullet) +playfield-render-offset-y+))
			   (type (bullet-type bullet)))
		   (case (bullet-family type)
			 ((:pellet)
			  (draw-sprite textures type render-x render-y :raywhite))
			 ((:small-star)
			  (draw-sprite-with-rotation textures type (mod (* frames 5.0) 360.0)
										 render-x render-y :raywhite))
			 (:none t))
		   (when show-hitboxes
			 (raylib:draw-circle-v (vec2 render-x render-y) (bullet-hit-radius type) :red)))))

(defconstant NUM-ENM 256)
(defstruct enm
  (type :none :type keyword)
  (x 0.0 :type float)
  (y 0.0 :type float)
  (health 0.0 :type real)
  (action-list nil)
  ;; todo consider removing or downsizing
  (extras (make-hash-table :size 4) :type hash-table))
(defvar live-enm
  (make-array NUM-ENM :element-type '(or null enemy) :initial-element nil))

(defun spawn-enemy (type x y health make-action-list)
  (let ((idx (position nil live-enm)))
	(unless idx
	  (error "No more open enemy slots!"))
	(let ((enm (setf (aref live-enm idx)
					 (make-enm :type type
							   :x x :y y
							   :health health))))
	  (setf (enm-action-list enm) (funcall make-action-list enm)))))

(defun delete-enemy (enm)
  (let ((idx (position enm live-enm)))
	(when idx
	  (setf (aref live-enm idx) nil))))

(defun tick-enemies ()
  (loop for enm across live-enm
		when enm
		  do (al:update (enm-action-list enm) 1)
			 (when (<= (enm-health enm) 0)
			   (raylib:play-sound (sebundle-enmdie sounds))
			   (delete-enemy enm))))

(defun process-collisions ()
  (loop for bullet across live-bullets
		if bullet
		  do (when (and (not (bullet-grazed bullet))
						(raylib:check-collision-circles
						 (vec2 player-x player-y) graze-radius
						 (vec2 (bullet-x bullet) (bullet-y bullet)) (bullet-hit-radius (bullet-type bullet))))
			   (incf graze)
			   (setf (bullet-grazed bullet) t)
			   (raylib:play-sound (sebundle-graze sounds))) ;; todo make this sound better?
			 (when (raylib:check-collision-circles
					(vec2 player-x player-y) hit-radius
					(vec2 (bullet-x bullet) (bullet-y bullet)) (bullet-hit-radius (bullet-type bullet)))
			   (raylib:play-sound (sebundle-playerdie sounds)))))

(defun force-clear-bullet-and-enemy ()
  (setf graze 0)
  (fill live-enm nil)
  (fill live-bullets nil))

(defun enm-hurtbox (enm)
  (case (enm-type enm)
	(:red-fairy (raylib:make-rectangle
				 ;; XXX why do we have to do this?? double-floats are creeping in
				 ;; which the ffi doesn't like. perhaps due to trig functions?
				 :x (coerce (- (enm-x enm) 16) 'single-float)
				 :y (coerce (- (enm-y enm) 16) 'single-float)
				 :width 32 :height 32))))

(defun draw-enemies (textures)
  (loop
	for enm across live-enm
	when enm
	  do (let ((render-x (+ (enm-x enm) +playfield-render-offset-x+))
			   (render-y (+ (enm-y enm) +playfield-render-offset-y+)))
		   (case (enm-type enm)
			 (:red-fairy
			  (draw-sprite textures :red-fairy render-x render-y :raywhite))
			 (:none t)))
		 (when show-hitboxes
		   (let ((hurtbox (enm-hurtbox enm)))
			 (when hurtbox
			   (incf (raylib:rectangle-x hurtbox) +playfield-render-offset-x+)
			   (incf (raylib:rectangle-y hurtbox) +playfield-render-offset-y+)
			   (raylib:draw-rectangle-rec hurtbox :red))))))

(defun shoot-at-player (srcpos)
  (let* ((player-pos (vec2 player-x player-y))
		 (diff (v- player-pos srcpos))
		 (facing (atan (vy2 diff) (vx2 diff))))
	(spawn-bullet :small-star-white
				  (vx2 srcpos) (vy2 srcpos)
				  facing 3.0
				  'bullet-control-linear)
	(raylib:play-sound (sebundle-shoot0 sounds))))

(defclass ease2d (al:basic)
  ((from-x :initarg :from-x)
   (from-y :initarg :from-y)
   (to-x :initarg :to-x)
   (to-y :initarg :to-y)
   (easer :initarg :easer))
  (:documentation
   "Update lambda receives the action and eased x/y values"))

(defmethod al:update ((action ease2d) dt)
  (let* ((progress (/ (+ (al:elapsed-time action) dt) (al:duration action)))
		 (eased (funcall (slot-value action 'easer) (clamp progress 0.0 1.0)))
		 (from-x (slot-value action 'from-x))
		 (from-y (slot-value action 'from-y))
		 (x (+ from-x
			   (* eased (- (slot-value action 'to-x) from-x))))
		 (y (+ from-y
			   (* eased (- (slot-value action 'to-y) from-y)))))
	(funcall (al::update-fun action) action x y)))

;; todo implement clone-into

(defconstant +movement-lane+ 1)
(defconstant +shooting-lane+ 2)

(defun pick-next-position (enm)
  (let* ((x (enm-x enm))
		 (y (enm-y enm))
		 (angle (random (* 2 pi))) ;; XXX(replays): rng access
		 (magnitude 50.0)
		 (dx (* magnitude (cos angle)))
		 (dy (* magnitude (sin angle)))
		 (nx (clamp (+ x dx) +playfield-min-x+ +playfield-max-x+))
		 (ny (clamp (+ y dy) +playfield-min-y+ +playfield-max-y+)))
	(values nx ny)))

(defun timed-move (enm)
  (make-instance
   'al:basic ;; todo: consider making a "oneshot" action type without the unnecessary overhead of basic
   :blocking t
   :duration 0
   :lanes +movement-lane+
   :update
   (lambda (self dt)
	 (multiple-value-bind (nx ny) (pick-next-position enm)
	   (al:push-back (make-instance
					  'ease2d
					  :from-x (enm-x enm) :from-y (enm-y enm)
					  :to-x nx :to-y ny
					  :lanes +movement-lane+
					  :easer 'ease-out-cubic :duration 90
					  :blocking t
					  :update (lambda (self nx ny)
								(setf (enm-x enm) nx)
								(setf (enm-y enm) ny)))
					 (al:action-list self))
	   (al:push-back (make-instance 'al:delay :duration 60 :lanes +movement-lane+)
					 (al:action-list self))
	   (al:push-back (timed-move enm) (al:action-list self))))))

(defstruct miscent
  (type :point :type keyword) ;; :point :bomb :life :bombfrag :lifefrag :mainshot
  (x 0.0 :type float)
  (y 0.0 :type float))

(defvar live-misc-ents
  (make-array 512 :element-type '(or null miscent) :initial-element nil))

(defun spawn-misc-ent (ent)
  (let ((idx (position nil live-misc-ents)))
	(unless idx
	  (error "No more open misc entity slots!"))
	(setf (aref live-misc-ents idx) ent)))

(defun delete-misc-ent (ent)
  (let ((idx (position ent live-misc-ents)))
	(when idx
	  ;; tolerate killing already-removed/dead
	  (setf (aref live-misc-ents idx) nil))))

(defun tick-misc-ents ()
  (loop for ent across live-misc-ents
		when ent
		  do 
			 (case (miscent-type ent)
			   (:mainshot
				(decf (miscent-y ent) 10.0)
				(let ((hitbox (raylib:make-rectangle
							   :x (- (miscent-x ent) 6) :y (- (miscent-y ent) 10)
							   :width 12 :height 16)))
				  (loop for enm across live-enm when enm
						do (when (raylib:check-collision-recs hitbox (enm-hurtbox enm))
							 (delete-misc-ent ent)
							 (decf (enm-health enm) 50)
							 (return))))
				;; todo damage dealing
				(when (< (miscent-y ent) +playfield-min-y+)
				  (delete-misc-ent ent)))
			   (t t))))

(defun draw-misc-ents (textures)
  (loop for ent across live-misc-ents when ent
		do (case (miscent-type ent)
			 (:mainshot
			  (let ((render-x (+ +playfield-render-offset-x+ (miscent-x ent)))
					(render-y (+ +playfield-render-offset-y+ (miscent-y ent))))
				(draw-sprite-with-rotation
				 textures :mainshot -90.0
				 render-x render-y :white)
				(when show-hitboxes
				  (let ((hitbox (raylib:make-rectangle
								 :x (- render-x 6) :y (- render-y 10)
								 :width 12 :height 16)))
					(raylib:draw-rectangle-rec hitbox :red))))))))

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
	  (incf player-yv 1))
	(when (and (raylib:is-key-down :key-z)
			   (zerop (mod frames 5))) ;; todo separate counter
	  (let ((y (- player-y 20)))
		(spawn-misc-ent (make-miscent :type :mainshot :x (- player-x 10) :y y))
		(spawn-misc-ent (make-miscent :type :mainshot :x (+ player-x 10) :y y))
		(raylib:play-sound (sebundle-playershoot sounds)))))

  ;; edge triggered stuff
  (loop for k = (raylib:get-key-pressed) then (raylib:get-key-pressed)
		while (not (eq :key-null k))
		do (case k
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
			  (spawn-enemy
			   :red-fairy
			   player-x
			   (- player-y 10) 200
			   (lambda (enm)
				 (make-instance
				  'al:action-list
				  :actions (list 
							(timed-move enm)
							(make-instance
							 'al:basic
							 :duration most-positive-fixnum
							 :lanes +shooting-lane+
							 :update (lambda (self dt)
									   (when (zerop (mod (al:elapsed-time self) 50))
										 (shoot-at-player
										  (vec2 (enm-x enm) (enm-y enm)))))))))))
			 (:key-y (force-clear-bullet-and-enemy)))))

(defun handle-player-movement ()
  (when (not (and (zerop player-xv) (zerop player-yv)))
    (let* ((delta-time (raylib:get-frame-time))
           (velocity (* player-speed delta-time (if (raylib:is-key-down :key-left-shift) 0.5 1)))
           (normalized-direction (vunit (vec2 player-xv player-yv)))
           (acceleration (v* normalized-direction velocity))
           (new-x (+ player-x (vx2 acceleration)))
           (new-y (+ player-y (vy2 acceleration))))
	  ;; todo: refine this so that you can bottomdrag instead of not moving at all
      (when (not (or (> new-x +playfield-max-x+) (> new-y +playfield-max-y+) (< new-y +playfield-min-y+) (< new-x +playfield-min-x+)))
        (setf player-x new-x)
        (setf player-y new-y)))
    (setf player-xv 0.0)
    (setf player-yv 0.0)))

(defvar focus-sigil-strength 0.0)
(defun draw-player (textures)
  (let* ((render-player-x (+ player-x +playfield-render-offset-x+))
		 (render-player-y (+ player-y +playfield-render-offset-y+)))
	;; player sprite (todo: directional moving sprites)
	(let ((x-texture-index (truncate (mod (/ frames 9) 8))))
	  (raylib:draw-texture-rec
	   (txbundle-reimu textures)
	   (raylib:make-rectangle :x (* 32 x-texture-index) :y 0 :width 32 :height 48)
	   (vec2 (- render-player-x 16) (- render-player-y 24))
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
	  (raylib:draw-circle-v (vec2 render-player-x render-player-y) graze-radius :green)
	  (raylib:draw-circle-v (vec2 render-player-x render-player-y) hit-radius :red))))

(defun render-all (textures)
  (raylib:clear-background bgcolor)
  (draw-player textures)

  (draw-enemies textures)
  (draw-misc-ents textures)
  (draw-bullets textures)

  (when paused
	(raylib:draw-text "PAUSED" 175 150 28 :purple))
  
  (raylib:draw-texture (txbundle-hud textures) 0 0 :raywhite)
  (raylib:draw-text (format nil "MISC: ~d" (- 512 (count nil live-misc-ents)))
					500 350
					18 :raywhite)
  (raylib:draw-text (format nil "GRAZE: ~d" graze)
					500 375
					18 :raywhite)
  (raylib:draw-text (format nil "ENM: ~d" (- NUM-ENM (count nil live-enm)))
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
		  (tick-misc-ents)
		  (process-collisions))
		(raylib:with-drawing
		  (render-all textures))
		(unless paused (incf frames)))
	  (unload-audio)
	  (unload-textures textures)
	  (unload-sfx))))
