(asdf:defsystem #:thdawn
  :depends-on (:cl-raylib :swank :cffi :cl-coroutine)
  :serial t
  :components ((:file "package")
			   (:file "main")))
