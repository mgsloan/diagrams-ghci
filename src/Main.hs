{-# LANGUAGE TemplateHaskell
           , StandaloneDeriving #-}
import ActiveHs.Simple

import Data.Label
import Prelude hiding ((.))
import Control.Category ((.))

import Data.Data
import Diagrams.Prelude hiding (text)
import Diagrams.Backend.Cairo
import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams
import Graphics.UI.Gtk.Toy.Text
import Graphics.UI.Gtk.Toy.Text.Interactive
import qualified Language.Haskell.Interpreter as Ghci

main = do
  chan <- startGHCiServer ["."] print print
  runToy $ State chan initialState mempty

data State = State
  { _chan :: TaskChan
  , _code :: MarkedText CursorMark
  , _result :: CairoDiagram
  }

$(mkLabels [''State])

plainText = monoText . (`MarkedText` ([] :: [(Ivl, CursorMark)]))

instance Diagrammable State Cairo R2 where
  toDiagram (State _ c r) = (plainText "> " ||| monoText c) === alignL r

instance Interactive State where
  display = displayDiagram (scaleY (-1) . margin . toDiagram)

  keyboard k _ s = update $ modify code (handleKey k) s

-- Needed in order to be able to provide a witness for CairoDiagram
deriving instance Typeable Any

update s = do
  val <- interpret (get chan s) "MyPrelude"
       $ Ghci.interpret (get (text . code) s) (mempty :: CairoDiagram)
  return $ set result (either (plainText . show) id val) s
