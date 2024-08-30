#!/usr/bin/chez --program
(import (chezscheme))

;; Automatically compile all dependency libraries
(compile-imported-libraries #t)
;; Generate wpo (chez term for LTO, mostly) files
(generate-wpo-files #t)
;; Do it 
(display (compile-program "main.ss"))
(newline)

;; Now we have the wpos, build the lto-ed binary
(display (compile-whole-program "main.wpo" "main_opt.so"))
(newline)

