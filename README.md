# thdawn

A Touhou-like STG made for the Autumn Lisp Game Jam 2023.

See roadmap.md for design and todos.

## Dev/Build Dependencies
* Chez Scheme
* Raylib dynamic library (from your package manager) and its recursive deps

## Runtime Dependencies
* Linux/macOS: Raylib dynamic library (from your package manager) and its recursive deps
* Windows: None (all deps to be packaged in distribution archive)

## Setup
All these steps are one time

1. Install Raylib and Chez Scheme
1. Clone this repo

## Run Interactively with a REPL
1. Install geiser and geiser-chez in emacs
1. C-c C-z to start a REPL
1. C-c C-b to eval the main.ss buffer
1. (fork-thread main) in the REPL to fork 
1. Spawn a game window in a thread by evaluating in the REPL `(fork-thread main)`.
   XXX: this won't work on macOS which requires first thread to be the one that does GUI stuff.

## Distribution Build
TODO: figure this out
