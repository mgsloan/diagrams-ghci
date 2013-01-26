{-# LANGUAGE
    MultiParamTypeClasses
  , ScopedTypeVariables
  , TupleSections
  , TypeFamilies
  , OverloadedStrings
  #-}

import Interpret

import ActiveHs.Simple             (TaskChan, startGHCiServer)
import Data.Default                (def)
import Diagrams.Backend.Cairo.Text ( textLineBounded )
import Control.Concurrent.Chan     (Chan, newChan, readChan, isEmptyChan)
import Control.Exception           (SomeException, catch)
import Filesystem.Path.CurrentOS   (extension, basename, encodeString)
import Graphics.UI.Toy.Prelude
import System.FSNotify             (WatchManager, Event(..), startManager, watchDirChan)


txt :: String -> CairoDiagram
txt = textLineBounded monoStyle

main :: IO ()
main = do
    chan <- startGHCiServer ["."] print print

    manager <- startManager
    fileChan <- newChan
    watchDirChan manager "." filePred fileChan
    runToy $ State chan manager fileChan mempty (txt startupMessage)
  where
    filePred (Modified x _) | extension x == Just "hse" = True
    filePred _ = False

    startupMessage = "Save a *.hse file in the current directory to cause redraw."
 

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
        `catch` \(err :: SomeException) -> return (setErr $ show err, False)
    where
      setResult path dia =
        s { _errMsg = mempty
          , _response = txt (encodeString $ basename path) === alignL (dia 0.0)
          }

      setErr err =
        s { _errMsg = txt err # fc darkred # alignL }

instance GtkDisplay State where
  display = displayDiagram dia
    where
      dia (State _ _ _ res err) = strutX 50 ||| (strutY 50 === inner) # reflectY
        where
          inner = blackLined $ vcat' (def {sep = 15}) [res, err]
