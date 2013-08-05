diagrams-ghci
=============

Installation
------------

Until diagrams-0.7 and my toy-* libraries are published to hackage, this repository requires the HEAD versions of those repositories.  The easiest way to get these dependencies is using [cabal-meta](https://github.com/yesodweb/cabal-meta) with [diagrams-sources](https://github.com/diagrams/diagrams-sources) and [toy-sources](https://github.com/mgsloan/toy-sources).

Usage
-----

Run `dist/buid/ghcdi/ghcdi` in the root directory.  Then, write a file with the extension `.hse`, for "Haskell Expression" to the same directory.  This change will be detected, and the expression will be rendered via diagrams.  

Notes
-----

"src/ActiveHs/Simple.hs" is from ActiveHs by Péter Diviánszky.  This is included because you can't use an application as a library!
