Editable Haskell expression yielding live-updating diagrams.

Dependencies:

```
git clone git@github.com:mgsloan/gtk-toy
git clone git@github.com:mgsloan/gtk-toy-diagrams

darcs get http://patch-tag.com/r/byorgey/diagrams-core
darcs get http://patch-tag.com/r/byorgey/diagrams-lib
darcs get http://patch-tag.com/r/byorgey/diagrams-cairo
```

Build:
```
cd diagrams-core
cabal install

cd ../diagrams-lib
cabal install

cd ../diagrams-cairo
cabal install

cd ../gtk-toy
cabal install

cd ../gtk-toy-diagrams
cabal install

cd ../diagrams-ghci
cabal install
```

All of the code except src/Main.hs aren't mine and come from the following
sources:

  - Hint by Daniel Gorin. For some reason beyond me Hint builds / installs
    yet refuses to be visible to GHC due to package issues.  So for now it's
    included.

  - ActiveHs by Péter Diviánszky.  This is included because you can't use
    an application as a library!  Also because of the Hint business.
