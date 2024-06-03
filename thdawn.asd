(asdf:defsystem #:thdawn
  :depends-on (:cl-raylib :swank :cl-coroutine :3d-vectors :action-list)
  :serial t
  :components ((:file "package")
			   (:file "easings")
			   (:file "textures")
			   (:file "sounds")
			   (:file "main")))
