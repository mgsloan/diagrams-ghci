{-# LANGUAGE
    TemplateHaskell
  , TypeFamilies
  , StandaloneDeriving #-}

import ActiveHs.Simple
import qualified Language.Haskell.Interpreter as Ghci

import Graphics.UI.Toy.Prelude
import Diagrams.Backend.Cairo (Cairo)

import Prelude hiding ((.))
import Control.Category ((.))
import Data.Label

import Data.Data

main = do
  chan <- startGHCiServer ["."] print print
  runToy $ State chan cursorText mempty mempty

data State = State
  { _chan :: TaskChan
  , _code :: MarkedText CursorMark
  , _response :: CairoDiagram
  , _err :: CairoDiagram
  }

type instance V State = R2

$(mkLabels [''State])

-- plainText = monoText . (`MarkedText` ([] :: [(Ivl, CursorMark)]))

txt :: String -> CairoDiagram
txt = drawText monoStyle . (plainText :: String -> MarkedText CursorMark)

instance Diagrammable Cairo State where
  diagram (State _ c r e) = (alignT $ txt "> " ||| drawText monoStyle c)
                          === 
                           alignL r
                          ===
                           alignL e

instance Interactive Gtk State where
  keyboard k _ s = update $ modify code (textKeyHandler k) s

instance GtkDisplay State where
  display = displayDiagram 
          ( scaleY (-1) . (strutX 50 |||) . (strutY 58 |||) . diagram)

-- Needed in order to be able to provide a witness for CairoDiagram.
deriving instance Typeable Any

update s = do
  val <- interpret (get chan s) "MyPrelude"
       $ Ghci.interpret (get (mText . code) s) (mempty :: CairoDiagram)
  return $ case val of
    Left e -> set err (txt $ show e) s
    Right x -> set err mempty $ set response x s
