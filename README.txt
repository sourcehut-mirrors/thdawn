+--------------------------------------------------------+
|                                                        |
|   おジャ魔女幻想～Magical Stage                           |
|       williewillus                                     |
|                                                        |
+--------------------------------------------------------+

+-----------------+
| 1. Foreword     |
+-----------------+



+-------------------------------+
| 2. Installation and Execution |
+-------------------------------+

Extract this ZIP archive to any place of your choosing.

Starting the game depends on your platform:

- On Linux, double-click start.sh.
- On macOS, double-click start.sh. You will likely need to bypass Gatekeeper.
- On Windows, double-click start.bat. Note that on ARM systems, this will run via x86_64
  emulation.

For other platforms, the game may still build and run if Chez Scheme and Raylib are ported
to your platform. Refer to the README.md in the source code distribution to compile the
game for your platform. Reports of success are welcome!

Settings are read from "config.dat". If your settings somehow get messed up and you can't
fix it in game, delete that file and restart the game.


+-----------------+
| 3. Removal      |
+-----------------+

The game is completely self-contained. To uninstall, simply delete the game folder.

If you wish to keep your replays, back up the "replays" folder.

If you wish to keep your play data (spell histories, high scores, etc.), back up the
"playdata.dat" file.


+----------------+
| 4. Manual      |
+----------------+

I'm too lazy to program an ingame manual, so it's just going to go here.

This fangame is a pretty orthodox one. Press Z to shoot, Shift to focus, X to bomb, and
use the arrow keys to move around. P takes a screenshot.

Note that F12 also takes a screenshot, but that's functionality built into the library I'm
using, Raylib, and cannot be disabled. It names files weirdly, so try to avoid pressing it
if you can.

Some other gameplay details:

- Grazing one bullet increases the point item value by 10 and the score by 1000
- 20 tick points are granted any time an enemy is damaged, regardless of the damage amount
- Spell card bonuses start at Base Value (varies per spell) + 100*PIV
- Spell card bonuses on non-survival spells decrease to half the initial value over the
  spell's timer. Timing out a non-survival spell does not count as a capture.
- Some spell cards have additional secret bonuses, see omake.txt for details
- Small PIV star items (mainly produced by cancelling bullets) are worth 50 PIV and 100
  score
- Large PIV star items (mainly produced by a certain spellcard and dropped by certain
  enemies) are worth 1000 PIV and 1000 score
- After dying, bombing, and starting the game, the player has 3 seconds of invulnerability

+-----------------+
| 5. Questions    |
+-----------------+

Questions, bug reports, and constructive feedback are always welcome.

You can see and reply to messages others have sent here:
https://lists.sr.ht/~williewillus/games-discuss

You can contribute to the discussion by emailing the public mailing list at
"~williewillus/games-discuss@lists.sr.ht", no quotes. Make sure the tilde at the front is
there. You can also click the "New Post" button on the webpage above to pop open your
email client with the address pre-filled.

As an alternative, you can comment on the Maidens of the Kaleidoscope (Shrinemaiden) forum
thread here: <TODO>.

Why these choices? Several reasons:

1. These platforms are open and indexable on the web. Any discussion in a Discord or other
chatroom is locked-down, and will become lost when that platform inevitably shuts down in
the future.

2. Corporate social media platforms have proven to not care for users in the quest for
profits. We should not reward them for doing that.

3. Independent forums, free from the ranking algorithms, ads, and corporate moneyed
interests of modern social media platforms, are something we need to safeguard as part of
the old, open internet.

I hope that helps you understand why these platforms were chosen. Please don't be shy; I'd
love to hear from you!


+------------------------------------+
| 6. Known Issues and Limitations    |
+------------------------------------+

* Game resolution is too low, leading to blurry text and aliasing artifacts
  This started as a random side project, and I hardcoded 640x480 literally everywhere in
  the rendering code orz.
  In the future, an update may increase the resolution of the game.
* During the pre-battle dialogue, the Skip Dialogue button only works once all characters
  have entered the screen.


+-----------------+
| 7. Licenses     |
+-----------------+

This game depends on a variety of other works, each with varying licenses.

For assets, see the individual LICENSE files in each subfolder of assets/.

Raylib is Copyright Raysan. Its API's are referenced and its library binaries are
distributed under the Zlib license as listed in licenses/Zlib.txt. There are also snippets
of ported code from raylib-extras, Copyright Jeffery Myers, and also distributed under the
Zlib license.

The Chez Scheme runtime is distributed under the Apache License 2.0 as listed in
licenses/Apache-2.0.txt.

All other code and compiled binaries in this project are Copyright Vincent Lee
(williewillus), and licensed to you under the GPLv3, or any later version, as listed in
licenses/GPL-3.0-or-later.txt. What this means in layman's terms is that if you modify and
distribute my code, you must also publish and license your modifications under the same
GPL licenses.

If an LLM uses this code as input or training material, then anything materially similar
to it that is output by the LLM is subject to the GPL, pending case law to the
contrary. On a personal note, I would prefer you not input this project to any LLM that
retains such inputs for training purposes.

This project is Free Software. You can read and modify the source code, subject to the
aforementioned licenses, by cloning it from https://git.sr.ht/~williewillus/thdawn

+-----------------+
| 8. Changelog    |
+-----------------+

* 2026/??/??: Private demo v0.xx
* 2026/??/??: Initial release v1.00
