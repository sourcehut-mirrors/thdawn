# Overall design
Current goal: Produce a single stage + bossfight script in a custom engine based on Chez
Scheme and Raylib.

Stage is to be themed roughly around music. BGM will be Ojamajo Carnival.

Midbosses: Doremi/Hazuki/Aiko/Onpu. Maybe Pop as midboss 2.

Boss: Alcyone from Magic Knight Rayearth (with banger boss theme tm)

## Immediate TODOs
* implement spawn delay and blurred pre-spawn rendering
  * vanilla touhou seems just render a xiaoyu but with an appropriate color and end size
	to the eventual bullet that's spawned. we can probably get away with the same.
* allow maximization of the window (with upscaled render)
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
* pan sfx based on horizontal position
* investigate if we want to adopt a system similar to the official Touhou games'
  "two-level" position and velocity system
