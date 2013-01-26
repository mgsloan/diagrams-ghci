{-# LANGUAGE
    TupleSections
  , TypeOperators
  , RankNTypes
  #-}
module SubDiagrams (txt, subDiagrams) where

import Interpret

import ActiveHs.Simple             (interpret, TaskChan)
import Control.Arrow               ((***), (&&&), second)
import Control.Monad               (liftM, join)
import Data.Char                   (isSpace)
import Data.Data                   (Data, gmapM, gmapQ)
import Data.Generics.Aliases       (extM, extQ)
import Data.Label
import Data.Maybe                  (fromJust, listToMaybe, catMaybes)
import Diagrams.Backend.Cairo.Text (queryCairo, getTextExtents)
import Graphics.UI.Toy.Prelude hiding (Name)
import Graphics.UI.Toy.Text        (monoStyle, plainText, drawText, MarkedText, CursorMark)

import Language.Haskell.Exts.Annotated

import qualified Control.Category as C
import qualified Control.Monad.State as ST
import qualified Data.Map as M

subDiagrams :: String -> TaskChan -> CairoDiagram
subDiagrams code tc =
  case parseExp code of
   (ParseOk expr) -> let (top, ds) = whereify expr in txt (prettyPrint top)
   (ParseFailed _ _) -> mempty
 where
  --exts = queryCairo (getTextExtents monoStyle)

type ExpS     = Exp     SrcSpanInfo
type DeclS    = Decl    SrcSpanInfo
type NameS    = Name    SrcSpanInfo 
type PatS     = Pat     SrcSpanInfo
type MatchS   = Match   SrcSpanInfo

-- Map of names to declarations
type DeclMap = M.Map String DeclS

-- Type used for internal state in whereification
type WST = ST.State ([String], [DeclS])

-- | "Whereify" the expression, giving names to all of the subtrees we're
-- intererested in.  The locations of these subtrees are replaced with a
-- reference to it.  Lambdas and case expressions turn into pattern matching,
-- with the subtrees of their contents appended in a where clause.  Giving
-- names to all of the parts of the expression allows us to build a tuple, as
-- the head expression, which references all of them, allowing us to get types
-- for each component in just one query to GHC.
whereify :: ExpS -> (ExpS, ([String], [DeclS]))
whereify top = -- prettyPrint *** (declMap . snd) $
              ST.runState (rec top) (manyNames, [])
 where
  gs = (`SrcSpanInfo` []) . fromJust . getSpan
  rec :: ExpS -> WST ExpS
  -- Lambdas create a declaration with an appended where clause
  rec l@(Lambda _ ps e) = do
    (ns, acc) <- mutate (second $ const []) 
    v <- rec e
    (_, bs) <- ST.get
    addDecl (gs l) v ps bs acc
  -- Applying a function to multiple parameters becomes a single  
  rec e@(App _ _ _) = addDecl' (gs e) 
                      =<< (liftM buildEApp . mapM rec $ splitEApp e)
  -- Variables get their own declaration
  rec v@(Var _ _) = addDecl' (gs v) v
  rec v@(Paren _ e) = rec e
  rec e = addDecl' (gs e) =<< grec e

  -- Generic recursion.
  grec :: forall a. Data a => a -> WST a
  grec = gmapM (grec `extM` rec)

  -- Adds a whereless / patternless declaration
  addDecl' srcsp e = do
    (_, acc) <- ST.get
    addDecl srcsp e [] [] acc
   
  addDecl :: SrcSpanInfo -> ExpS -> [PatS] -> [DeclS] -> [DeclS] -> WST ExpS
  addDecl srcsp v ps bs acc = do
    (n:ns, _) <- ST.get
    ST.put . (ns,) . (:acc) $
      (FunBind srcsp [ Match sp (Ident sp n) ps (UnGuardedRhs sp
                         $ if null bs 
                           then v 
                           else Let sp (BDecls sp bs) v) Nothing ])
    return $ mkPlain n


-- Utilities grab bag

mkPlain :: String -> ExpS
mkPlain n = (Var sp (UnQual sp (Ident sp n)))

declMap :: [DeclS] -> DeclMap
declMap = M.fromList . map (get funName &&& id)

funMatches :: DeclS :-> [MatchS]
funMatches = lens getMatches setMatches
 where
  getMatches (FunBind _ ms) = ms
  setMatches ms (FunBind a _) = FunBind a ms

funName :: DeclS :-> String
funName = lens getName setName C.. funMatches
 where
  getName ((Match _ (Ident _ n) _ _ _):_) = n
  setName n = map (\(Match a (Ident b _) c d e) ->
                     Match a (Ident b n) c d e)

manyNames :: [String]
manyNames = ["__" ++ filter (not . isSpace) [a, b, c] | c <- az, b <- az, a <- tail az ]
 where
  az = ' ' : ['a'..'z']

getSpan :: (Data a) => a -> Maybe SrcSpan
getSpan = listToMaybe . catMaybes
        . gmapQ (const Nothing `extQ` (Just . srcInfoSpan))

splitEApp :: ExpS -> [ExpS]
splitEApp = reverse . helper
 where
  helper (App _ a b) = b : helper a
  helper t = [t]

buildEApp :: [ExpS] -> ExpS
buildEApp = helper . reverse
 where
  helper [t] = t
  helper (b : a) = App sp (helper a) b

mutate :: ST.MonadState s m => (s -> s) -> m s
mutate f = do
  x <- ST.get
  ST.put (f x)
  return x