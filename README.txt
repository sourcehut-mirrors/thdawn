+--------------------------------------------------------+
|                                                        |
|   東方???～Touhou Magical Stage                         |
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

+-----------------+
| 4. Questions    |
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

+--------------------+
| 5. Known Issues    |
+--------------------+

* Game resolution is too low, leading to blurry text and aliasing artifacts
  This started as a random side project, and I hardcoded 640x480 literally everywhere in
  the rendering code orz.
  In the future, an update may increase the resolution of the game.

+-----------------+
| 6. Changelog    |
+-----------------+

* 2026/??/??: Private demo v0.xx
* 2026/??/??: Initial release v1.00
