(in-package :thdawn)

;; Easing functions for movement, graphics, etc.
;; All inputs and outputs in [0, 1]

(defun ease-out-cubic (x)
  (- 1 (expt (- 1 x) 3.0)))
