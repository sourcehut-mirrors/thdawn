# Overall design
Current goal: Produce a single stage + bossfight script in a custom engine based on Chez
Scheme and Raylib.

Stage is to be themed roughly around music. BGM will be Ojamajo Carnival.

Midbosses: Doremi/Hazuki/Aiko/Onpu. Maybe Pop as midboss 2.

Boss: Alcyone from Magic Knight Rayearth (with banger boss theme tm)

## Immediate TODOs
* design multi-task abstractions so that an enemy can e.g. move and shoot independently
* design how chapters and stage flow will work in both a prod build and in-dev
  * ability to find and kill individual tasks in coro.ss
  * chapters are individual functions that tail call each other, driven by a single task.
	As opposed to a main task that calls the chapters sequentially.
	This will allow starting at individual chapters for debugging.
	Debug will need ability to kill current "stage driver" task (or maybe just all tasks),
	reset the music, and start the right chapter.
* fix z-ordering of bullet rendering, currently arbitrary based on bullet array order
  * needs to be later bullets always on top
* implement enemy dying effect
* implement graze particle
* implement first chapter of stage to flesh out API's needed
  * and all bullet types needed for it
* spell tracking and hud
* boss position marker
* boss aura
* boss spell circle
* implement gameover
* dialogue system
* player and boss portraits
* implementing directional moving sprites for the player
* replays
* make sure we're doing proper alpha blending (premultiplication and such)
* find a way to avoid having to add render-offset-x and y everywhere.
  * for example, draw to a separate framebuffer for gameplay area then move that whole thing
* lasers
* pan sfx based on horizontal position
* investigate if we want to adopt a system similar to the official Touhou games'
  "two-level" position and velocity system
