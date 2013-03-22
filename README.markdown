Editable Haskell expression yielding live-updating diagrams.

Dependencies (sorry for the inconvenience - hope to get these on hackage soon!):

```
git clone https://github.com/mgsloan/toy-interface.git
git clone https://github.com/mgsloan/toy.git
git clone https://github.com/mgsloan/toy-gtk-diagrams.git
git clone https://github.com/mgsloan/marked-text.git

# Probably not currently necessary - v0.6 is now on hackage.
git clone https://github.com/diagram/diagrams-core.git
git clone https://github.com/diagram/diagrams-lib.git
git clone https://github.com/diagram/diagrams-cairo.git
```

Build:
```
cd diagrams-core
cabal install

cd ../diagrams-lib
cabal install

cd ../diagrams-cairo
cabal install

cd ../toy-interface
cabal install

cd ../gtk-toy
cabal install

cd ../gtk-toy-diagrams
cabal install

cd ../marked-text
cabal install

cd ../diagrams-ghci
cabal install
```

"src/ActiveHs/Simple.hs" is from ActiveHs by Péter Diviánszky.  This is included because you can't use an application as a library!
