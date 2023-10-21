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
4. Clone raylib bindings into `~/common-lisp` (it's not on the quicklisp package repo): https://github.com/longlene/cl-raylib
5. Load the raylib bindings using quicklisp, which will fetch and load all of its recursive deps: `(ql:quickload :cl-raylib)` in your lisp repl
6. Fetch and load other project deps: `(ql:quickload :cl-coroutine)`, `(ql:quickload :swank)`

Once we specify a proper system definition for this project these steps won't be so manual, but for now that's how it is.

TODO(williewilus): Do that

## Run Interactively with a REPL
1. Install SLIME in emacs https://slime.common-lisp.dev/
2. `sbcl --load main.lisp` (or equivalent for your lisp implementation), which will start the game program and a REPL server
3. In emacs, `M-x slime-connect` and hit enter, which will connect a REPL to the live running game
4. In the repl, type `(in-package :thdawn)` to enter the namespace of the game.
5. Now you can evaluate things in the game as it runs, examine game state, reload individual functions, etc. Magic!

## Distribution Build
TODO(williewillus) figure it out
