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
1. Spawn a game window in a thread by evaluating in the REPL `(fork-thread main)`.
   XXX: this won't work on macOS which requires first thread to be the one that does GUI stuff.

## Distribution Build
Run the `build.ss` program, which will create the output artifact `thdawn.boot`. On Unix
systems you can do this by running `make`.

Distribute this with the assets, a copy of Chez Scheme, and raylib's dynamic library. This
step is probably going to be manual.

## About the Design
### About the Code Quality

This program is intentionally "badly written", in that I do not care much about
proper abstraction beyond what is necessary, encapsulation, implementation hiding, etc.
I do not care about properly "modularizing" or splitting up the program either.

Why? Because at its core this project is meant to be a creative work, and a way of
challenging myself with making something new. If I spend all my time making the code
look nice, that's time taken away from actually implementing core engine features or the
game logic itself.

### Colocation of Code

Somewhat related, is that this project intentionally colocates the "bullet pattern code"
and the "engine code". There is no API separation like Danmakufu (C++ vs DNH), LuaSTG (C++
vs Lua), or even the official Touhou games (C++ vs ECL). This too is in the name of
development speed and creativity (though it potentially has execution speed benefits too
from not having to cross an abstraction boundary for enemy and bullet control). A spell
wants to reach directly into the core internals of the engine?  Sure! Why not? In this
way, the "engine" is meant to be used, hacked on, and customized for each individual
project. This is similar to how the official Touhou games' engine has evolved over the
years.

### Why Chez Scheme?
Because Common Lisp doesn't have first-class continuations, which are a boon for writing
enemy and bullet patterns in a straight-line and functional manner. The alternative is to
create objects and state variables representing each possible control pattern, or using
something clunky like action lists. With continuations, all state variables are neatly
captured and encapsulated as local variables in the continuation's stack frames, without
the syntactic and performance overhead of putting them in the fields of a structure.

No, CL-CONT does not count. It's a cheap imitation of first-class continuations because it
uses macro-rewriting. This means a coroutine cannot call into a series of helper functions
that eventually does a yield, because yield is only valid syntax in a macro-transformed
function.

Chez Scheme provides enough of CL's interactivity and performance that sacrificing the rest
of CL's niceties (larger standard library, CLOS, SLIME, etc.) is well worth it in order
to gain continuations.

### Why Ojamajo Doremi?
I started watching this series around early 2023 or late 2022, I've forgotten. Ever since
then, I've daydreamed many times of setting the opening theme to danmaku, while walking
down Capitol Hill in Seattle on the way to work. This project is taking the baby steps
towards realizing that dream.
