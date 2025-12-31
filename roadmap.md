# Overall design
Current goal: Produce a single stage + bossfight script in a custom engine based on Chez
Scheme and Raylib.

Stage is to be themed roughly around music. BGM will be Ojamajo Carnival.

Midbosses: Doremi/Hazuki/Aiko/Onpu. Maybe Pop as midboss 2.

Boss: Doremi/Hazuki/Aiko/Onpu. BGM: Naisho yo ojamajo (no syncing)

## Immediate TODOs
* replay saving/playback
* replay menu
* dialogue system (localization support)
* player and boss portraits
* make sure we're doing proper alpha blending (premultiplication and such)
* find a way to avoid having to add render-offset-x and y everywhere.
  * for example, draw to a separate framebuffer for gameplay area then move that whole thing
* pan sfx based on horizontal position?

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

## boss design brainstorming

Midsp doremi
"My First Spellcard"
Something related to the second poron (spinny music notes in a bubble)
Something related to the certification gems (3 xiaoyu)

Ring of pellets/small stars with 3 notes inside -> unit
Aya non1/non2 style?

Midsp2 doremi
Easy spinning + mentos or rest fan from boss

Sp1 group
Something related to the "getting dressed on time" quirk in season 1. Failure means you die
Funny would be to have different art once reimu "transforms"
Kind of a pre fight midsp 3 but it would happen after most of the dialogue.

music: naisho yo ojamajo

Sp1 Doremi 
Something flower or music themed normal dodging
Harukaze: alternate waves of spring final ish stuff and "wind blowing" danmaku 
But that might be too annoying to dodge 
Nons: Music notes that bounce similar to in the stage

Sp2 Hazuki
love themed? crush on yada idk
incorporate homing from ojamajo doremi shoot?
Something about fans/traditional dance?
Shoot mid orbs+trail, the trail expands into a fan, then the fan bullets get shot outwards. Plus something from the boss to add some more action.
Nons: butterflies and swirls

Sp3 Aiko
"straightforward" or "simple"
incorporate direct shot from ojamajo doremi shoot?
lasers + micro?

Pretty active/lots of boss movement
Jump around to set mines, when triggered fire done aimed stuff and some decoration rings
Chase player maybe?
Nons: Hearts

Sp4 Trio
something where bullets from each char interact with each other
(might require some refactors like bullet tagging, etc.)

Sp5 Doremi (gimmick start spells here)
Something steak related
Probably drawn by bullets instead of custom bullet type
Fireballs? Bc searing steak lmao
Some sort of "collect all the steak(some item)" or else something bad happens.

Sp6 Hazuki
Something related to wisps/ghosts/fear of ghosts
Could be the stereotypical kill the ghost familiars that chase you around and they explode either when killed or after a timeout
Or we could do a CIV thing where the ghosts attack and we have to save her haha

Sp7 Aiko

Something like pong/soccer/penalty shootout

Probably simpler penalty shootout (each wave have her kick in random direction for three
waves, bomb can delete the ball for insurance), since it's easier than making a pong or
air hockey AI lol

Sp8 Trio/survival (let's just do 60 seconds...)
osu catch the fruit???? Lol
Position lock, dynamic sync for the music

Sp9 Trio
Magical Stage
make sure to loop music so that "thank you!!" only plays after the bosses are defeated
might require some clever trickery (or hardcoding...)

Scenario
Doremi and gang somehow get spirited off to gensokyo (took a wrong door on the way to the
magic world)
Teach reimu about "their way" of magic.
