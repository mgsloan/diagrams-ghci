{-# LANGUAGE
    MultiParamTypeClasses
  , ScopedTypeVariables
  , TupleSections
  , TypeFamilies
  , OverloadedStrings
  #-}

import Interpret

import Prelude hiding (catch)
import ActiveHs.Simple             (TaskChan, startGHCiServer)
import Control.Concurrent.Chan     (Chan, newChan, readChan, isEmptyChan)
import Control.Exception           (SomeException(..), catch)
import Filesystem.Path.CurrentOS   (extension, basename, encodeString)
import Graphics.UI.Toy.Gtk.Prelude
import System.FSNotify             ( WatchManager, Event(..), startManager
                                   , watchDirChan)

main :: IO ()
main = do
    chan <- startGHCiServer ["."] print print

    manager <- startManager
    fileChan <- newChan
    watchDirChan manager "." filePred fileChan
    runToy $ State chan manager fileChan mempty (preText startupMsg)
  where
    filePred (Modified x _) | extension x == Just "hse" = True
    filePred _ = False

    startupMsg = "Save a *.hse file in the current directory to cause redraw."

data State = State
  { _chan        :: TaskChan
  , _manager     :: WatchManager
  , _fileChan    :: Chan Event
  , _response    :: CairoDiagram
  , _errMsg      :: CairoDiagram
  }

type instance V State = R2

instance Interactive Gtk State where
  tick _ s = do
      let files = _fileChan s
          tasks = _chan s

      isEmpty <- isEmptyChan files

      let refresh = do
            (Modified path _) <- readChan files
            let filename = encodeString path
            code <- readFile filename
            val <- ghcdiInterpret tasks code
            return (either setErr (setResult path) val, True)

      if isEmpty then return (s, False) else refresh
        `catch` \(SomeException err) -> return (setErr $ show err, False)
    where
      setResult path dia =
        s { _errMsg = mempty
          , _response =
              preText (encodeString $ basename path)
                ===
              strutY 20
                ===
              alignL (dia 0.0)
          }

      setErr err =
        s { _errMsg = preText err # fc darkred # alignL }

instance GtkDisplay State where
  display = displayDiagram dia
    where
      dia (State _ _ _ res err) =
          strutX 50 ||| reflectY (strutY 50
                                    ===
                                  inner)
        where
          inner = blackLined $ res === err
