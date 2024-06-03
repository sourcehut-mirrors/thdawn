;; Declares the thdawn namespace and brings all `use`d bindings into
;; (unqualified) scope within it
(defpackage :thdawn
  (:use :cl :cl-coroutine :3d-vectors)
  (:local-nicknames (:al #:org.shirakumo.fraf.action-list))
  (:export :main))
