# thdawn

A Touhou-like STG made for the Autumn Lisp Game Jam 2023.

See roadmap.md for design and todos.

## Dependencies
* Common Lisp
* Raylib dynamic library (from your package manager)

## Setup
All these steps are one time

1. Install a common lisp implementation
2. Install quicklisp into it https://www.quicklisp.org/beta/
3. Clone this repo into `~/common-lisp`
4. Clone the raylib bindings into `~/common-lisp`: `git clone
   https://github.com/williewillus/cl-raylib`. This library isn't available on the
   quicklisp package repo yet.
4. In a lisp repl, evaluate: `(ql:quickload "thdawn")`, which will fetch all other recursive
   dependencies from the internet.

## Run Interactively with a REPL
1. Install SLIME in emacs https://slime.common-lisp.dev/
2. `sbcl --eval "(require 'thdawn)" --eval "(in-package :thdawn)" --eval "(main)" --quit`
   (or your equivalent lisp implementation of choice), which will start the game program
   and a REPL server. For convenience, this command has been put in `run.sh`.
3. In emacs, `M-x slime-connect` and hit enter, which will connect a REPL to the live running game
4. In the repl, type `(in-package :thdawn)` to enter the namespace of the game.
5. Now you can evaluate things in the game as it runs, examine game state, reload individual functions, etc. Magic!

## Distribution Build
TODO(williewillus) figure it out
