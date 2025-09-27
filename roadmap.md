# Overall design
Current goal: Produce a single stage + bossfight script in a custom engine based on Chez
Scheme and Raylib.

Stage is to be themed roughly around music. BGM will be Ojamajo Carnival.

Midbosses: Doremi/Hazuki/Aiko/Onpu. Maybe Pop as midboss 2.

Boss: Doremi/Hazuki/Aiko/Onpu. BGM: Half Point (no syncing)

## Immediate TODOs
* implement gameover
* dialogue system
* player and boss portraits
* replays
* make sure we're doing proper alpha blending (premultiplication and such)
* find a way to avoid having to add render-offset-x and y everywhere.
  * for example, draw to a separate framebuffer for gameplay area then move that whole thing
* pan sfx based on horizontal position

## Stage design brainstorming

chapter 0: basic columns of fairies down the sides x 4 waves
end of the intro: 5 lasers from small fairies,
big fairy draws a treble clef which spreads out

chapter 1:
big fairy swing into view, circular spattering of bullets
do~shio! long notes have aimed fairies from the side
or on the accented notes have fairies pop in and shoot some rings

chapter 2:
??

chapter 3:
big+small fairy trains, fan or circle patterns or both
"bubble" parts fire bubble bullets from adhoc fairies
buildup to chorus: mini-death fairy/"surprise" pattern that seems scary but is easy

chapter 4: midspell. Something spinny.

chapter 5: something "rainy" or "starry", fairies firing gravity-affected bullets/emulate
meteor shower?

chapter 6: ch0 reprise/remix

chapter 7: ch1 remix

chapter 8: ?? ch2 remix ?

chapter 9: ch3 remix

chapter 10: ??

chapter 11: midspell 2

chapter 12: all out!
