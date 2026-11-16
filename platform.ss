;; Copyright (C) 2025 Vincent Lee; GPL-3.0-or-later
(library (platform)
  (export move-file)
  (import (chezscheme))

  (define is-windows
	(let* ([t (symbol->string (machine-type))]
		   [len (string-length t)])
	  (and (>= len 2)
		   (string=? "nt" (substring t (- len 2) len)))))
  (define (move-file old new)
	;; atomic rename on proper OS'es, dumb delete-rename on Windows (for now?)
	(when is-windows
	  (delete-file new #f))
	(rename-file old new)))
