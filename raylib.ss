(library (raylib)
  (export init-window close-window window-should-close set-target-fps get-frame-time
		  begin-drawing end-drawing clear-background draw-circle-v
		  draw-text draw-fps draw-rectangle-rec
		  init-audio-device close-audio-device
		  load-music-stream play-music-stream stop-music-stream pause-music-stream
		  resume-music-stream seek-music-stream update-music-stream unload-music-stream
		  load-sound play-sound unload-sound
		  load-texture unload-texture draw-texture-rec draw-texture draw-texture-pro
		  is-key-down get-key-pressed set-exit-key
		  push-matrix pop-matrix translatef rotatef)
  (import (chezscheme) (geom))

  ;; just calling load-shared-object is not permitted here
  ;; as r6rs requires all definitions to precede any expressions
  ;; r6rs does guarantee the library body executes top to bottom though,
  ;; so this is the right place to do it
  (define _dummy (load-shared-object "libraylib.so")) ;; todo windows and macos

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
  (define (load-global-color rgba)
	(define r (bitwise-and #xff (bitwise-arithmetic-shift-right rgba 24)))
	(define g (bitwise-and #xff (bitwise-arithmetic-shift-right rgba 16)))
	(define b (bitwise-and #xff (bitwise-arithmetic-shift-right rgba 8)))
	(define a (bitwise-and #xff rgba))
	(ftype-set! Color (r) global-color r)
	(ftype-set! Color (g) global-color g)
	(ftype-set! Color (b) global-color b)
	(ftype-set! Color (a) global-color a))
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

  (define init-window
	(foreign-procedure "InitWindow" (int int string) void))

  (define close-window
	(foreign-procedure "CloseWindow" () void))

  (define window-should-close
	(foreign-procedure "WindowShouldClose" () boolean))

  (define set-target-fps
	(foreign-procedure "SetTargetFPS" (int) void))

  (define begin-drawing
	(foreign-procedure "BeginDrawing" () void))

  (define end-drawing
	(foreign-procedure "EndDrawing" () void))

  (define clear-background0
	(foreign-procedure "ClearBackground" ((& Color)) void))
  (define (clear-background rgba)
	(load-global-color rgba)
	(clear-background0 global-color))


  (define draw-circle-v0
	(foreign-procedure "DrawCircleV" ((& RayVector2) float (& Color)) void))
  (define (draw-circle-v x y radius rgba)
	(load-global-vector2 x y)
	(load-global-color rgba)
	(draw-circle-v0 global-vector2 radius global-color))

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
	  (_ boolean)
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
  (define unload-sound0
	(foreign-procedure "UnloadSound" ((& Sound)) void))
  (define (unload-sound sound)
	(unload-sound0 sound)
	(foreign-free (ftype-pointer-address sound)))
  
  (define draw-rectangle-rec0
	(foreign-procedure "DrawRectangleRec" ((& RayRect) (& Color)) void))
  (define (draw-rectangle-rec x y width height rgba)
	(load-global-rectangle global-rectangle x y width height)
	(load-global-color rgba)
	(draw-rectangle-rec0 global-rectangle global-color))

  (define draw-text0
	(foreign-procedure "DrawText" (string int int int (& Color)) void))
  (define (draw-text text x y font-size rgba)
	(load-global-color rgba)
	(draw-text0 text x y font-size global-color))

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
	(load-global-color rgba)
	(draw-texture-rec0 tex global-rectangle global-vector2 global-color))

  (define draw-texture0
	(foreign-procedure "DrawTexture" ((& Texture) int int (& Color)) void))
  (define (draw-texture tex x y rgba)
	(load-global-color rgba)
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
	(load-global-color rgba)
	(draw-texture-pro0 tex global-rectangle global-rectangle2
					   global-vector2 rotation global-color))

  ;; HACK: If I type this as boolean, as the Raylib header states,
  ;; I get issues where is-key-down always returns true.
  ;; It seems like there's some bug, and searching on the Internet
  ;; reveals some other people that run into this as well.
  ;; The hack here is to treat the ABI as unsigned char and do
  ;; the zero-comparison ourselves.
  ;; TODO: test that this works on all the main platforms
  (define is-key-down0
	(foreign-procedure "IsKeyDown" (int) unsigned-8))
  (define (is-key-down k)
	(not (zero? (is-key-down0 k))))

  (define get-key-pressed
	(foreign-procedure "GetKeyPressed" () int))

  (define set-exit-key
	(foreign-procedure "SetExitKey" (int) void))

  (define get-frame-time
	(foreign-procedure "GetFrameTime" () float))

  (define push-matrix
	(foreign-procedure "rlPushMatrix" () void))
  (define pop-matrix
	(foreign-procedure "rlPopMatrix" () void))
  (define translatef
	(foreign-procedure "rlTranslatef" (float float float) void))
  (define rotatef
	(foreign-procedure "rlRotatef" (float float float float) void))
  )
