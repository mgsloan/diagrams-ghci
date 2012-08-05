{-# LANGUAGE
    TemplateHaskell
  , TupleSections
  , TypeFamilies
  #-}

import Interpret
import SubDiagrams

import ActiveHs.Simple        (TaskChan, startGHCiServer)
import Data.Char              (isSpace)
import Data.Function          (on)
import Data.Label             (get, set, modify, mkLabels)
import Data.List              (groupBy, find)
import Diagrams.Backend.Cairo (Cairo)
import Control.Monad          (join, liftM)
import Control.Monad.Error    (catchError)
import Graphics.UI.Toy.Prelude hiding (debug)

import qualified Graphics.UI.Gtk.General.General as G
import qualified Language.Haskell.Interpreter as Ghci
import qualified Control.Category as C

paragraphs
  = map (reverse . dropWhile isSpace . reverse)
  . filter (not . all isSpace) . map unlines . groupBy ((==) `on` null) . lines

main = do
  chan <- startGHCiServer ["."] print print
  hist <- (reverse . paragraphs) <$> readFile "history"
  runToy $ State chan cursorText mempty mempty mempty 0 hist

data State = State
  { _chan        :: TaskChan
  , _code        :: MarkedText CursorMark
  , _response    :: Double -> CairoDiagram
  , _annotations :: CairoDiagram
  , _errMsg      :: CairoDiagram
  , _delayTicks  :: Int
  , _history     :: [String]
  }

type instance V State = R2

$(mkLabels [''State])

-- plainText = monoText . (`MarkedText` ([] :: [(Ivl, CursorMark)]))

--TODO use CatOpts

instance Diagrammable Cairo State where
  diagram (State _ c r a e _ h) = vcat
    [ alignT $ txt "> " ||| (drawText monoStyle c === a)
    , strutY 15
    , alignL (r 0.0)
    , strutY 15
    , alignL e
    , strutY 15
    , txt "(Clipboard) Alt +" # fc blue
    ]

tickLimit = 4

historyLabels = map (head . show) [0..9] ++ ['a'..'z']

doKbd k s = (if get delayTicks s > tickLimit then update else return)
          $ modify code (textKeyHandler (True, k)) s

instance Interactive Gtk State where
  keyboard (True, Right k) i s

    | k == 's' && eitherHeld "Control" i = do

      let text = get (mText C.. code) s
      appendFile "history" ("\n\n" ++ text)
        `catchError` const (return ())
      return $ modify history (text:) s

    |             eitherHeld "Alt"     i = update $
      case find ((k ==) . fst) $ zip historyLabels (get history s) of
        Just (_, code') -> set code (addText cursorText (plainText code')) s
        Nothing -> s

    | otherwise = doKbd (Right k) s

  keyboard (True, l) _ s = doKbd l s
  keyboard _ _ s = return s

  tick _ s = return $ (modify delayTicks (+1) s, False)

instance GtkDisplay State where
  display dw i s = disp dw i s
--    `catch` (\e -> displayDiagram (txt . show) dw i e >> return s)
   where
    disp = displayDiagram
         ( scaleY (-1) . (strutX 50 |||) . (strutY 58 |||) . (=== hist) . diagram )
    hist
      | eitherHeld "Alt" i
        = vcat
        $ zipWith (|||) (map (fc blue . txt . (:":  ")) historyLabels)
                        (map txt $ get history s)
      | otherwise
        = mempty

update s = do
  val <- ghcdiInterpret tc code_text
  return
    . set delayTicks 0
    $ case val of
      Left  e -> set errMsg (txt e # fc darkred) s
      Right x -> set errMsg mempty
--               . set annotations (subDiagrams code_text tc)
               $ set response x s
 where
  tc = get chan s
  code_text = get (mText C.. code) s
