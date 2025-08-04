;; The config is just an alist. This is because nongenerative records can't handle old
;; data when new fields are added.

(library (config)
  (export read-config save-config)
  (import (chezscheme))
  
  (define default-config
	;; todo: key-config, 
	'((music-vol . 100)
	  (sfx-vol . 85)))
  (define config-path "config.dat")

  (define (read-config)
	(guard (e [(i/o-file-does-not-exist-error? e)
			   (with-output-to-file config-path
				 (lambda () (pretty-print default-config)))
			   default-config])
	  (with-input-from-file config-path
		read)))

  (define (save-config data)
	(with-output-to-file config-path
	  (lambda () (pretty-print data))
	  'truncate)))
