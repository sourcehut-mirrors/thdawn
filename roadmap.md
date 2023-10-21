# Overall design
Intended to be a Touhou-like STG, similar in spirit and simplicity to the old PC98 games,
but with modern developments like focus mode and spell cards.

Three stages planned, each with a boss.

Assignee in [brackets]

# Art stuff
* player and boss sprites
* game HUD
* better fonts for text/number rendering
* stage 1 music
* stage 1 boss music
* stage 2 music
* stage 2 boss music
* stage 3 music
* stage 3 boss music
* stage 1 background
* stage 2 background
* stage 3 background
* Boss aura
* Spellcard aura

# Engine stuff
* Basic bullet engine and framework for spawning, simulating, manipulating, and managing bullets [willie]
* REPL setup for bullet pattern experimentation without restarting the game [willie]
* Main menu
* replays (stretch goal)

# Gameplay stuff
* Scoring system design
  * Probably just keep this basic, get score by point items, extra lives by score
* Stage 1 pattern design
* Stage 1 boss design
* Stage 2 pattern design
* Stage 2 boss design
* Stage 3 pattern design
* Stage 3 boss design

# Workspace stuff
No prefixed imports, keep everything in a global namespace and prefix the names
themselves. This makes REPL live coding much easier since you don't have to worry about
prefixing things when overriding a function with a new implementation, for example.

So do `(varfn stage1-boss-pattern-1 ...)` in stage1.janet instead of `(varfn boss-pattern-1 ...)`
in stage1.janet.

Use defn for engine/framework stuff you don't expect to change while live-coding, while
attacks and patterns and stuff should be varfn. If unsure, use varfn for everything, we
can change them to defns closer to release for performance.
