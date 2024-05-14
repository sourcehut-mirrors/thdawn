(asdf:defsystem #:thdawn
  :depends-on (:cl-raylib :swank :cffi :cl-coroutine :3d-vectors)
  :serial t
  :components ((:file "package")
			   (:file "textures")
			   (:file "sounds")
			   (:file "main")))
