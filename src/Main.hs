{-# LANGUAGE TemplateHaskell
           , StandaloneDeriving #-}

import ActiveHs.Simple
import qualified Language.Haskell.Interpreter as Ghci

import Graphics.UI.Gtk.Toy.Prelude
import Diagrams.Backend.Cairo (Cairo)

import Prelude hiding ((.))
import Control.Category ((.))
import Data.Label

import Data.Data

main = do
  chan <- startGHCiServer ["."] print print
  runToy $ State chan cursorText mempty

data State = State
  { _chan :: TaskChan
  , _code :: MarkedText CursorMark
  , _response :: CairoDiagram
  }

$(mkLabels [''State])

plainText = monoText . (`MarkedText` ([] :: [(Ivl, CursorMark)]))

instance Diagrammable State Cairo R2 where
  toDiagram (State _ c r) = (alignT $ plainText "> " ||| monoText c)
                         === 
                            alignL r

instance Interactive State where
  keyboard k _ s = update $ modify code (textKey k) s

instance GtkInteractive State where
  display = displayDiagram 
          ( scaleY (-1) . (strutX 50 |||) . (strutY 58 |||) . toDiagram)

-- Needed in order to be able to provide a witness for CairoDiagram.
deriving instance Typeable Any

update s = do
  val <- interpret (get chan s) "MyPrelude"
       $ Ghci.interpret (get (mText . code) s) (mempty :: CairoDiagram)
  return $ set response (either (plainText . show) id val) s
