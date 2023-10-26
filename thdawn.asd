(asdf:defsystem #:thdawn
  :depends-on (:cl-raylib :swank :cffi :3d-vectors)
  :serial t
  :components ((:file "package")
			   (:file "main")))
