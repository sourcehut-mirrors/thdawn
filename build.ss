#!/usr/bin/chez --program
(import (chezscheme))

;; Automatically compile all dependency libraries
(compile-imported-libraries #t)
;; Generate wpo (chez term for LTO, mostly) files
(generate-wpo-files #t)

;; Do the main compilation
(display (compile-program "main.ss"))
(newline)

;; Now we have the wpos, build the lto-ed binary
;; Library wpos are automatically detected
(display "Compiling WPO-ed program\n")
(display (compile-whole-program "main.wpo" "main_wpo.so"))
(newline)

;; Create the final boot file
(display "Building final boot file\n")
(make-boot-file "thdawn.boot" '("petite") "main_wpo.so")
(display "Done, run `chez -B thdawn.boot` to launch\n")
