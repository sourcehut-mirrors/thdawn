;; Copyright (C) 2025 Vincent Lee; GPL-3.0-or-later
(library (raylib)
  (export init-window close-window window-should-close set-target-fps get-frame-time
		  toggle-borderless-windowed toggle-fullscreen
		  set-window-min-size get-screen-height get-screen-width
		  begin-texture-mode end-texture-mode
		  load-render-texture unload-render-texture render-texture-inner
		  begin-drawing end-drawing clear-background draw-circle-v draw-line
		  draw-circle-lines-v
		  draw-text draw-text-ex measure-text-ex draw-fps
		  draw-rectangle-rec draw-rectangle-pro
		  draw-rectangle-gradient-h draw-rectangle-gradient-v
		  draw-ring draw-ring-lines
		  init-audio-device close-audio-device
		  load-music-stream play-music-stream stop-music-stream pause-music-stream
		  resume-music-stream seek-music-stream update-music-stream unload-music-stream
		  set-music-volume set-music-looping
		  load-sound play-sound unload-sound set-sound-pitch set-sound-volume
		  load-texture unload-texture draw-texture-rec draw-texture draw-texture-pro
		  load-font unload-font
		  is-key-down get-key-pressed set-exit-key is-key-pressed is-key-released
		  set-trace-log-level
		  push-matrix pop-matrix with-matrix translatef rotatef scalef
		  rlbegin rlend vertex2 texcoord color4f color4ub normal3f set-texture
		  set-texture-filter
		  set-shapes-texture get-shapes-texture get-shapes-texture-rectangle
		  draw-spline-bezier-quadratic draw-spline-bezier-cubic take-screenshot)
  (import (chezscheme) (geom))

  ;; just calling load-shared-object is not permitted here
  ;; as r6rs requires all definitions to precede any expressions
  ;; r6rs does guarantee the library body executes top to bottom though,
  ;; so this is the right place to do it
  (define _dummy
	(case (machine-type)
	  [(i3nt ti3nt a6nt ta6nt arm64nt tarm64nt)
	   (load-shared-object "raylib.dll")]
	  [(i3osx ti3osx a6osx ta6osx arm64osx tarm64osx ppc32osx tppc32osx)
	   (load-shared-object "libraylib.dylib")]
	  [else (load-shared-object "libraylib.so")]))

  ;; For foreign structs that Raylib handles by-value, we just allocate
  ;; one location and use it to pass and return things from ffi by-value
  ;; This avoids thrashing foreign-alloc and foreign-free, as many of these functions
  ;; are called multiple times per frame, per object.
  ;; Note this assumes only one thread makes Raylib calls at a time, which
  ;; is the case since our entire application is single-threaded
  ;; (outside the REPL, which shouldn't be making Raylib calls)

  (define-ftype Color
	(struct (r unsigned-8) (g unsigned-8) (b unsigned-8) (a unsigned-8)))
  (define global-color
	(make-ftype-pointer Color (foreign-alloc (ftype-sizeof Color))))
  (define global-color2
	(make-ftype-pointer Color (foreign-alloc (ftype-sizeof Color))))
  (define (load-global-color dest rgba)
	(define r (bitwise-and #xff (bitwise-arithmetic-shift-right rgba 24)))
	(define g (bitwise-and #xff (bitwise-arithmetic-shift-right rgba 16)))
	(define b (bitwise-and #xff (bitwise-arithmetic-shift-right rgba 8)))
	(define a (bitwise-and #xff rgba))
	(ftype-set! Color (r) dest r)
	(ftype-set! Color (g) dest g)
	(ftype-set! Color (b) dest b)
	(ftype-set! Color (a) dest a))
  (define (unload-global-color)
	(define r (ftype-ref Color (r) global-color))
	(define g (ftype-ref Color (g) global-color))
	(define b (ftype-ref Color (b) global-color))
	(define a (ftype-ref Color (a) global-color))
	(bitwise-ior (bitwise-arithmetic-shift-left r 24)
				 (bitwise-arithmetic-shift-left g 16)
				 (bitwise-arithmetic-shift-left b 8)
				 a))

  (define-ftype RayVector2
	(struct (x float) (y float)))
  (define global-vector2
	(make-ftype-pointer RayVector2 (foreign-alloc (ftype-sizeof RayVector2))))
  (define (load-global-vector2 x y)
	(ftype-set! RayVector2 (x) global-vector2 (inexact x))
	(ftype-set! RayVector2 (y) global-vector2 (inexact y)))
  (define (unload-global-vector2)
	(define x (ftype-ref RayVector2 (x) global-vector2))
	(define y (ftype-ref RayVector2 (y) global-vector2))
	(values x y))

  (define-ftype RayRect
	(struct (x float) (y float) (width float) (height float)))
  (define global-rectangle
	(make-ftype-pointer RayRect (foreign-alloc (ftype-sizeof RayRect))))
  ;; Two of them, because some calls take two :D
  (define global-rectangle2
	(make-ftype-pointer RayRect (foreign-alloc (ftype-sizeof RayRect))))
  (define (load-global-rectangle dest x y width height)
	(ftype-set! RayRect (x) dest (inexact x))
	(ftype-set! RayRect (y) dest (inexact y))
	(ftype-set! RayRect (width) dest (inexact width))
	(ftype-set! RayRect (height) dest (inexact height)))
  (define (unload-global-rectangle src)
	(make-rectangle (ftype-ref RayRect (x) src)
					(ftype-ref RayRect (y) src)
					(ftype-ref RayRect (width) src)
					(ftype-ref RayRect (height) src)))

  (define init-window
	(foreign-procedure "InitWindow" (int int string) void))

  (define close-window
	(foreign-procedure "CloseWindow" () void))

  (define toggle-borderless-windowed
	(foreign-procedure "ToggleBorderlessWindowed" () void))
  (define toggle-fullscreen
	(foreign-procedure "ToggleFullscreen" () void))
  (define set-window-min-size
	(foreign-procedure "SetWindowMinSize" (int int) void))
  (define get-screen-width
	(foreign-procedure "GetScreenWidth" () int))
  (define get-screen-height
	(foreign-procedure "GetScreenHeight" () int))

  (define window-should-close
	(foreign-procedure "WindowShouldClose" () stdbool))

  (define set-target-fps
	(foreign-procedure "SetTargetFPS" (int) void))

  (define begin-drawing
	(foreign-procedure "BeginDrawing" () void))

  (define end-drawing
	(foreign-procedure "EndDrawing" () void))

  (define begin-texture-mode
	(foreign-procedure "BeginTextureMode" ((& RenderTexture)) void))
  (define end-texture-mode
	(foreign-procedure "EndTextureMode" () void))

  (define clear-background0
	(foreign-procedure "ClearBackground" ((& Color)) void))
  (define (clear-background rgba)
	(load-global-color global-color rgba)
	(clear-background0 global-color))

  (define draw-line0
	(foreign-procedure "DrawLine" (int int int int (& Color)) void))
  (define (draw-line sx sy ex ey rgba)
	(load-global-color global-color rgba)
	(draw-line0 sx sy ex ey global-color))

  (define draw-circle-v0
	(foreign-procedure "DrawCircleV" ((& RayVector2) float (& Color)) void))
  (define (draw-circle-v x y radius rgba)
	(load-global-vector2 x y)
	(load-global-color global-color rgba)
	(draw-circle-v0 global-vector2 radius global-color))

  (define draw-circle-lines-v0
	(foreign-procedure "DrawCircleLinesV" ((& RayVector2) float (& Color)) void))
  (define (draw-circle-lines-v x y radius rgba)
	(load-global-vector2 x y)
	(load-global-color global-color rgba)
	(draw-circle-lines-v0 global-vector2 radius global-color))

  (define draw-rectangle-gradient-v0
	(foreign-procedure "DrawRectangleGradientV"
					   (int int int int (& Color) (& Color)) void))
  (define (draw-rectangle-gradient-v x y w h rgba-upper rgba-lower)
	(load-global-color global-color rgba-upper)
	(load-global-color global-color2 rgba-lower)
	(draw-rectangle-gradient-v0 x y w h global-color global-color2))
  (define draw-rectangle-gradient-h0
	(foreign-procedure "DrawRectangleGradientH"
					   (int int int int (& Color) (& Color)) void))
  (define (draw-rectangle-gradient-h x y w h rgba-left rgba-right)
	(load-global-color global-color rgba-left)
	(load-global-color global-color2 rgba-right)
	(draw-rectangle-gradient-h0 x y w h global-color global-color2))

  (define draw-ring0
	(foreign-procedure "DrawRing" ((& RayVector2) float float
								   float float int (& Color)) void))
  (define (draw-ring x y rinner router start-ang end-ang segments rgba)
	(load-global-vector2 x y)
	(load-global-color global-color rgba)
	(draw-ring0 global-vector2 rinner router start-ang end-ang segments global-color))

  (define draw-ring-lines0
	(foreign-procedure "DrawRingLines" ((& RayVector2) float float
										float float int (& Color)) void))
  (define (draw-ring-lines x y rinner router start-ang end-ang segments rgba)
	(load-global-vector2 x y)
	(load-global-color global-color rgba)
	(draw-ring-lines0 global-vector2 rinner router
					  start-ang end-ang segments global-color))

  (define init-audio-device
	(foreign-procedure "InitAudioDevice" () void))
  (define close-audio-device
	(foreign-procedure "CloseAudioDevice" () void))

  ;; ABI as of Raylib 5.0-1
  (define-ftype AudioStream
	(struct
	  (_ uptr)
	  (_ uptr)
	  (_ unsigned-int)
	  (_ unsigned-int)
	  (_ unsigned-int)))
  (define-ftype Music
	(struct
	  (_ AudioStream)
	  (_ unsigned-int)
	  (looping stdbool)
	  (_ int)
	  (_ void*)))
  
  (define load-music-stream0
	(foreign-procedure "LoadMusicStream" (string) (& Music)))
  (define (load-music-stream file)
	(define result (make-ftype-pointer Music (foreign-alloc (ftype-sizeof Music))))
	(load-music-stream0 result file)
    result)
  (define play-music-stream
	(foreign-procedure "PlayMusicStream" ((& Music)) void))
  (define stop-music-stream
	(foreign-procedure "StopMusicStream" ((& Music)) void))
  (define pause-music-stream
	(foreign-procedure "PauseMusicStream" ((& Music)) void))
  (define resume-music-stream
	(foreign-procedure "ResumeMusicStream" ((& Music)) void))
  (define seek-music-stream
	(foreign-procedure "SeekMusicStream" ((& Music) float) void))
  (define update-music-stream
	(foreign-procedure "UpdateMusicStream" ((& Music)) void))
  (define unload-music-stream0
	(foreign-procedure "UnloadMusicStream" ((& Music)) void))
  (define set-music-volume
	(foreign-procedure "SetMusicVolume" ((& Music) float) void))
  (define (set-music-looping music looping)
	(ftype-set! Music (looping) music looping))
  (define (unload-music-stream music)
	(unload-music-stream0 music)
	(foreign-free (ftype-pointer-address music)))

  (define-ftype Sound
	(struct
	  (stream AudioStream)
	  (frame-count unsigned-int)))
  (define load-sound0
	(foreign-procedure "LoadSound" (string) (& Sound)))
  (define (load-sound file)
	(define result (make-ftype-pointer Sound (foreign-alloc (ftype-sizeof Sound))))
	(load-sound0 result file)
	result)
  (define play-sound
	(foreign-procedure "PlaySound" ((& Sound)) void))
  (define set-sound-volume
	(foreign-procedure "SetSoundVolume" ((& Sound) float) void))
  (define set-sound-pitch
	(foreign-procedure "SetSoundPitch" ((& Sound) float) void))
  (define unload-sound0
	(foreign-procedure "UnloadSound" ((& Sound)) void))
  (define (unload-sound sound)
	(unload-sound0 sound)
	(foreign-free (ftype-pointer-address sound)))
  
  (define draw-rectangle-rec0
	(foreign-procedure "DrawRectangleRec" ((& RayRect) (& Color)) void))
  (define (draw-rectangle-rec x y width height rgba)
	(load-global-rectangle global-rectangle x y width height)
	(load-global-color global-color rgba)
	(draw-rectangle-rec0 global-rectangle global-color))

  (define draw-rectangle-pro0
	(foreign-procedure "DrawRectanglePro" ((& RayRect) (& RayVector2)
										   float (& Color)) void))
  (define (draw-rectangle-pro x y width height ox oy rot rgba)
	(load-global-rectangle global-rectangle x y width height)
	(load-global-vector2 ox oy)
	(load-global-color global-color rgba)
	(draw-rectangle-pro0 global-rectangle
						 global-vector2
						 rot
						 global-color))

  (define draw-text0
	(foreign-procedure "DrawText" (string int int int (& Color)) void))
  (define (draw-text text x y font-size rgba)
	(load-global-color global-color rgba)
	(draw-text0 text x y font-size global-color))
  (define draw-text-ex0
	(foreign-procedure "DrawTextEx" ((& Font) string (& RayVector2)
									 float float (& Color)) void))
  (define (draw-text-ex font text x y size spacing rgba)
	(load-global-vector2 x y)
	(load-global-color global-color rgba)
	(draw-text-ex0 font text global-vector2 size spacing global-color))
  (define measure-text-ex0
	(foreign-procedure "MeasureTextEx" ((& Font) string float float) (& RayVector2)))
  (define (measure-text-ex font text size spacing)
	(measure-text-ex0 global-vector2 font text size spacing)
	(unload-global-vector2))

  (define draw-fps
	(foreign-procedure "DrawFPS" (int int) void))

  (define-ftype Texture
	(struct
	  (id unsigned-int)
	  (width int)
	  (height int)
	  (mipmaps int)
	  (format int)))
  (define load-texture0
	(foreign-procedure "LoadTexture" (string) (& Texture)))
  (define (load-texture file)
	(define result (make-ftype-pointer Texture (foreign-alloc (ftype-sizeof Texture))))
	(load-texture0 result file)
	result)
  (define unload-texture0
	(foreign-procedure "UnloadTexture" ((& Texture)) void))
  (define (unload-texture tex)
	(unload-texture0 tex)
	(foreign-free (ftype-pointer-address tex)))

  (define-ftype RenderTexture
	(struct
	  [id unsigned-int]
	  [texture Texture]
	  [depth Texture]))

  (define load-render-texture0
	(foreign-procedure "LoadRenderTexture" (int int) (& RenderTexture)))
  (define (load-render-texture width height)
	(define result (make-ftype-pointer RenderTexture
									   (foreign-alloc (ftype-sizeof RenderTexture))))
	(load-render-texture0 result width height)
	result)
  (define unload-render-texture0
	(foreign-procedure "UnloadRenderTexture" ((& RenderTexture)) void))
  (define (unload-render-texture rtexture)
	(unload-render-texture0 rtexture)
	(foreign-free (ftype-pointer-address rtexture)))

  (define (render-texture-inner rtexture)
	(ftype-&ref RenderTexture (texture) rtexture))
  
  (define draw-texture-rec0
	(foreign-procedure "DrawTextureRec"
					   ((& Texture) (& RayRect)
						(& RayVector2) (& Color)) void))
  (define (draw-texture-rec tex rect v rgba)
	(load-global-rectangle
	 global-rectangle
	 (rectangle-x rect) (rectangle-y rect)
	 (rectangle-width rect) (rectangle-height rect))
	(load-global-vector2 (v2x v) (v2y v))
	(load-global-color global-color rgba)
	(draw-texture-rec0 tex global-rectangle global-vector2 global-color))

  (define draw-texture0
	(foreign-procedure "DrawTexture" ((& Texture) int int (& Color)) void))
  (define (draw-texture tex x y rgba)
	(load-global-color global-color rgba)
	(draw-texture0 tex x y global-color))

  (define draw-texture-pro0
	(foreign-procedure "DrawTexturePro"
					   ((& Texture) (& RayRect) (& RayRect)
						(& RayVector2) float (& Color))
					   void))
  (define (draw-texture-pro tex src dest origin rotation rgba)
	(load-global-rectangle
	 global-rectangle
	 (rectangle-x src) (rectangle-y src)
	 (rectangle-width src) (rectangle-height src))
	(load-global-rectangle
	 global-rectangle2
	 (rectangle-x dest) (rectangle-y dest)
	 (rectangle-width dest) (rectangle-height dest))
	(load-global-vector2 (v2x origin) (v2y origin))
	(load-global-color global-color rgba)
	(draw-texture-pro0 tex global-rectangle global-rectangle2
					   global-vector2 rotation global-color))

  (define-ftype Font
	(struct
	  (_ int)
	  (_ int)
	  (_ int)
	  (_ Texture)
	  (_ void*)
	  (_ void*)
	  (_ unsigned-int)))
  (define load-font0
	(foreign-procedure "LoadFont" (string) (& Font)))
  (define (load-font file)
	(define result (make-ftype-pointer Font (foreign-alloc (ftype-sizeof Font))))
	(load-font0 result file)
    result)
  (define unload-font0
	(foreign-procedure "UnloadFont" ((& Font)) void))
  (define (unload-font font)
	(unload-font0 font)
	(foreign-free (ftype-pointer-address font)))

  (define is-key-down
	(foreign-procedure "IsKeyDown" (int) stdbool))

  (define is-key-pressed
	(foreign-procedure "IsKeyPressed" (int) stdbool))

  (define is-key-released
	(foreign-procedure "IsKeyReleased" (int) stdbool))

  (define get-key-pressed
	(foreign-procedure "GetKeyPressed" () int))

  (define set-exit-key
	(foreign-procedure "SetExitKey" (int) void))

  (define get-frame-time
	(foreign-procedure "GetFrameTime" () float))

  (define set-trace-log-level
	(foreign-procedure "SetTraceLogLevel" (int) void))

  (define push-matrix
	(foreign-procedure "rlPushMatrix" () void))
  (define pop-matrix
	(foreign-procedure "rlPopMatrix" () void))
  (define-syntax with-matrix
	(syntax-rules ()
	  [(_ e1 e2 ...)
	   (begin
		 (push-matrix)
		 e1 e2 ...
		 (pop-matrix))]))
  (define translatef
	(foreign-procedure "rlTranslatef" (float float float) void))
  (define rotatef
	(foreign-procedure "rlRotatef" (float float float float) void))
  (define scalef
	(foreign-procedure "rlScalef" (float float float) void))
  (define rlbegin
	(foreign-procedure "rlBegin" (int) void))
  (define rlend
	(foreign-procedure "rlEnd" () void))
  (define vertex2
	(foreign-procedure "rlVertex2f" (float float) void))
  (define texcoord
	(foreign-procedure "rlTexCoord2f" (float float) void))
  (define set-texture0
	(foreign-procedure "rlSetTexture" (unsigned-int) void))
  (define (set-texture tex)
	(set-texture0 (if tex (ftype-ref Texture (id) tex) 0)))
  (define color4f
	(foreign-procedure "rlColor4f" (float float float float) void))
  (define color4ub
	(foreign-procedure "rlColor4ub" (unsigned-8 unsigned-8 unsigned-8 unsigned-8) void))
  (define normal3f
	(foreign-procedure "rlNormal3f" (float float float) void))

  (define set-shapes-texture0
	(foreign-procedure "SetShapesTexture" ((& Texture) (& RayRect)) void))
  (define (set-shapes-texture tex rect)
	(load-global-rectangle global-rectangle
						   (rectangle-x rect) (rectangle-y rect)
						   (rectangle-width rect) (rectangle-height rect))
	(set-shapes-texture0 tex global-rectangle))

  (define global-texture
	(make-ftype-pointer Texture (foreign-alloc (ftype-sizeof Texture))))
  (define get-shapes-texture0
	(foreign-procedure "GetShapesTexture" () (& Texture)))
  (define (get-shapes-texture)
	(get-shapes-texture0 global-texture)
	global-texture)
  (define get-shapes-texture-rectangle0
	(foreign-procedure "GetShapesTextureRectangle" () (& RayRect)))
  (define (get-shapes-texture-rectangle)
	(get-shapes-texture-rectangle0 global-rectangle)
	(unload-global-rectangle global-rectangle))

  (define set-texture-filter
	(foreign-procedure "SetTextureFilter" ((& Texture) int) void))

  ;; room for 32 vec2s
  (define spline-buf-length 32)
  (define spline-draw-buf (make-bytevector (* 2 spline-buf-length (ftype-sizeof float))))
  (define (load-spline-buf points)
	(define num-points (vector-length points))
	(assert (<= num-points spline-buf-length))
	(let loop ([points-idx 0]
			   [buf-idx 0])
	  (unless (>= points-idx num-points)
		(let ([point (vector-ref points points-idx)])
		  (bytevector-ieee-single-native-set! spline-draw-buf buf-idx (v2x point))
		  (bytevector-ieee-single-native-set! spline-draw-buf
											  (+ buf-idx (ftype-sizeof float))
											  (v2y point)))
		(loop (add1 points-idx) (+ buf-idx (* 2 (ftype-sizeof float)))))))
  
  (define draw-spline-bezier-quadratic0
	(foreign-procedure "DrawSplineBezierQuadratic" (u8* int float (& Color)) void))
  (define (draw-spline-bezier-quadratic points thick rgba)
	(load-spline-buf points)
	(load-global-color global-color rgba)
	(draw-spline-bezier-quadratic0 spline-draw-buf (vector-length points) thick global-color))

  (define draw-spline-bezier-cubic0
	(foreign-procedure "DrawSplineBezierCubic" (u8* int float (& Color)) void))
  (define (draw-spline-bezier-cubic points thick rgba)
	(load-spline-buf points)
	(load-global-color global-color rgba)
	(draw-spline-bezier-cubic0 spline-draw-buf (vector-length points) thick global-color))

  (define take-screenshot
	(foreign-procedure "TakeScreenshot" (string) void))
  )
