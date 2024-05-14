# thdawn

A Touhou-like STG made for the Autumn Lisp Game Jam 2023.

See roadmap.md for design and todos.

## Dev/Build Dependencies
* Common Lisp
* Raylib dynamic library (from your package manager) and its recursive deps

## Runtime Dependencies
* Linux/macOS: Raylib dynamic library (from your package manager) and its recursive deps
* Windows: None (all deps to be packaged in distribution archive)

## Setup
All these steps are one time

1. Install a common lisp implementation
2. Install quicklisp into it https://www.quicklisp.org/beta/
3. Clone this repo into `~/common-lisp`
4. Clone the raylib bindings into `~/common-lisp`: `git clone
   https://github.com/longlene/cl-raylib`. This library isn't available on the
   quicklisp package repo yet.
4. In a lisp repl, evaluate: `(ql:quickload "thdawn")`, which will fetch all other recursive
   dependencies from the internet.

## Run Interactively with a REPL
1. Install SLIME in emacs https://slime.common-lisp.dev/
2. Run `M-x slime` to start a REPL
3. In the REPL, do `(ql:quickload "thdawn")` to load the project and deps, and then
   `(in-package :thdawn)` to enter the project namespace.
4. Spawn a game window in a thread by evaluating in the REPL e.g. `(sb-thread:make-thread #'main)`.
   XXX: this won't work on macOS which requires first thread to be the one that does GUI stuff.
5. Now you can evaluate things in the game as it runs, examine game state, reload individual functions, etc. Magic!

## Distribution Build
Linux/macOS: Run `build.sh` which will put the output binary at `thdawn`.

TODO: Port to windows and windows build
