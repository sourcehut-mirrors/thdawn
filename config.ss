;; Copyright (C) 2025 Vincent Lee; GPL-3.0-or-later
;; The config is just an alist. This is because nongenerative records can't handle old
;; data when new fields are added.
(library (config)
  (export reset-config read-config save-config)
  (import (chezscheme))

  (include "keyconsts.ss")
  
  (define default-config
	`((music-vol . 100)
	  (sfx-vol . 85)
	  (keybindings . ((up . ,key-up)
					  (down . ,key-down)
					  (left . ,key-left)
					  (right . ,key-right)
					  (focus . ,key-left-shift)
					  (shoot . ,key-z)
					  (bomb . ,key-x)
					  (pause . ,key-escape)
					  (screenshot . ,key-p)
					  (quick-restart . ,key-r)
					  (quick-quit . ,key-q)
					  (skip-dialogue . ,key-left-control)))
	  (gamepad-id . 0)
	  (x-deadzone . 0.2)
	  (y-deadzone . 0.2)
	  (padmappings . ((up . 1) (right . 2)
					  (down . 3) (left . 4)
					  (focus . 11) (shoot . 7)
					  (bomb . 6) (pause . 15)))))
  (define config-path "config.dat")

  (define (read-config)
	(guard (e [(i/o-file-does-not-exist-error? e)
			   (save-config default-config)
			   (read-config)])
	  (with-input-from-file config-path
		read)))

  (define (reset-config)
	(save-config default-config)
	(read-config))

  (define (save-config data)
	(with-output-to-file "config.dat.tmp"
	  (lambda () (pretty-print data))
	  'truncate)
	(rename-file "config.dat.tmp" config-path)))
