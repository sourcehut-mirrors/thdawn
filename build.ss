#!/usr/bin/chez --program
(import (chezscheme))

;; Automatically compile all dependency libraries
(compile-imported-libraries #t)
;; Generate wpo (chez term for LTO, mostly) files
(generate-wpo-files #t)
(when (getenv "PROD")
  (optimize-level 3))

;; Do the main compilation
(define libs-used (compile-program "main.ss"))
(display "Libraries used: ")
(display libs-used)
(newline)
(newline)

;; Now we have the wpos, build the lto-ed binary
;; Library wpos are automatically detected
(display "Compiling WPO-ed program\n")
(define missing-wpo-libs (compile-whole-program "main.wpo" "main_wpo.so"))
(when (not (null? missing-wpo-libs))
  (display missing-wpo-libs)
  (newline))
;; must be empty, meaning that all scheme libraries have been found and bundled
;; into the wpo-ed object file
(assert (null? missing-wpo-libs))
(newline)

;; Create the final boot file
(display "Building final boot file\n")
(make-boot-file "thdawn.boot" '("petite") "main_wpo.so")
(display "Done, run `chez -B thdawn.boot` to launch\n")
