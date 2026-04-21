+--------------------------------------------------------+
|                                                        |
|   東方???～Touhou Magical Stage                         |
|       williewillus                                     |
|                                                        |
+--------------------------------------------------------+

+-----------------+
| 1. 前言          |
+-----------------+



+-----------------+
| 2. 安裝與運行     |
+-----------------+

將遊戲ZIP解壓到任意文件夾。

啓動遊戲方法：

- Linux：點擊 start.sh.
- macOS: 點擊 start.sh. 可能需要關掉 Gatekeeper.
- Windows: 點擊 start.bat. 注意：ARM系統將會通過x86_64 emulation來運行

其他平臺也許也能將遊戲編譯、運行。前提是Chez Scheme和Raylib兩者都必須能在該平臺運行。細節
請查看源代碼README.md，成功的話也請你告訴我喔！

設定將從“config.dat”載入，如果設定卡了不能在遊戲內修復，將config.dat刪掉然後重啓遊戲。

+-----------------+
| 3. 卸裝          |
+-----------------+

刪掉遊戲文件夾即可。

想保留replay請備份“replays”文件夾。

想保留符卡收率、高分等數據請備份“playdata.dat”。

+-----------------+
| 4. 有問題的話     |
+-----------------+

歡迎玩家們通過電郵提供問題、bug和友善的反饋。

可以到這邊查看之前的提問郵件：
https://lists.sr.ht/~williewillus/games-discuss

自己也可以至郵"~williewillus/games-discuss@lists.sr.ht"。（也可以打開以上網頁然後點擊“new
post”）

另外也可以在Maidens of the Kaleidoscope (Shrinemaiden) 論壇發帖：TODO

+--------------------+
| 5. 已知問題          |
+--------------------+

* 遊戲分辨率過低，導致多方畫面很糊
  原本是做着玩玩，沒想到竟然能做完，代碼中很多地方已經以640x480爲分辨率定下了orz
  將來有可能出補丁修復這個問題

+-----------------+
| 6. 開源協議等      |
+-----------------+

本作
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
licenses/GPL-3.0.txt.

This project is Free Software. You can read and modify the source code, subject to the
aforementioned licenses, by cloning it from https://git.sr.ht/~williewillus/thdawn

+-----------------+
| 7. 更新紀錄       |
+-----------------+

* 2026/??/??: 內測版 v0.xx
* 2026/??/??: 正式版 v1.00
