{-# LANGUAGE
    DeriveDataTypeable
  , ScopedTypeVariables
  , StandaloneDeriving
  #-}
module Interpret where
import ActiveHs.Simple             (TaskChan, interpret)
import Control.Applicative         ((<$>))
import Control.Monad               (join, liftM)
import Data.Generics               (Data, Typeable, listify, everywhere, extT)
import Data.Maybe                  (catMaybes)
import Debug.Trace                 (trace)
import Graphics.UI.Toy.Gtk.Prelude (CairoDiagram, Any)
import Language.Haskell.Interpreter (set, languageExtensions, OptionVal(..), availableExtensions)
import qualified Language.Haskell.Interpreter as Ghci
import Language.Haskell.Exts.Annotated

-- Needed in order to be able to provide a witness for CairoDiagram.
deriving instance Typeable Any

errorText :: Ghci.InterpreterError -> String
errorText (Ghci.UnknownError err) = "Unk: " ++ err
errorText (Ghci.WontCompile es) = unlines $ map Ghci.errMsg es
errorText (Ghci.NotAllowed e) = "Not Allowed: " ++ e
errorText (Ghci.GhcException e) = "GHC error: " ++ e

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left  x) = Left $ f x
mapLeft _ (Right y) = Right y

sp :: SrcSpanInfo
sp = SrcSpanInfo (SrcSpan "" 0 0 0 0) [] --error "srcspan"

debug x = trace (show x) x
pdebug x = trace (prettyPrint x) x

ghcdiInterpret :: TaskChan -> String -> IO (Either String (Double -> CairoDiagram))
ghcdiInterpret tc code = do
  result <- join . mapLeft errorText <$> interpret tc "MyPrelude" process
  case result of
    (Left e) -> do
      fb <- fallback
      either (return . (const $ Left e)) (return . Right) fb
    (Right x) -> return $ Right x
 where
  extFilter (Ghci.UnknownExtension _) = False
  extFilter x | x `elem` [Ghci.NoImplicitPrelude, Ghci.RebindableSyntax] = False
              | otherwise                                                = True
  process = do
    set [languageExtensions := filter extFilter availableExtensions]
    type_text <- Ghci.typeOf code
    case parseTypeWithMode parseMode type_text of
      (ParseOk ty) -> Right <$>
        exec (debug ("ghcdiShow ((\n" ++ fixCode code ++ ") :: " ++ processType ty ++ ")"))
      (ParseFailed _ err) -> return $ Left ("Type parse error: " ++ err)

  fallback = interpret tc "MyPrelude" . exec
           $ "const (\n" ++ fixCode code ++ ") :: Double -> CairoDiagram"

  fixCode = unlines . map (' ':) . lines

  processType ty = prettyPrint $ case ty of
    (TyForall l tvs (Just ctx) t)
      -> let assts = getAssts ctx
             subs = catMaybes $ map (liftM prettyPrint . diagramAsst) assts
             t' = everywhere (id `extT` doSub subs) t
             ctx' = CxTuple sp . catMaybes $ map (processAsst $ allTyVars t') assts
          in TyForall l tvs (Just ctx') t'
    _ -> ty

  doSub l t | prettyPrint t `elem` l = TyVar sp (Ident sp "CairoDiagram")
            | otherwise = t

  diagramAsst :: Asst SrcSpanInfo -> Maybe (Type SrcSpanInfo)
  diagramAsst (ClassA _ (UnQual _ (Ident _ "PathLike")) [t]) = Just t
  diagramAsst _ = Nothing

  processAsst :: [String] -> Asst SrcSpanInfo -> Maybe (Asst SrcSpanInfo)
  processAsst ts x
    | all (`notElem` ts) (allTyVars x) = Nothing
    | otherwise = Just x

  allTyVars :: forall a. Data a => a -> [String]
  allTyVars = map (prettyPrint :: Type SrcSpanInfo -> String) . listify isTyVar

  isTyVar (TyVar _ _) = True
  isTyVar _ = False

  exec txt = Ghci.interpret txt (undefined :: Double -> CairoDiagram)

getAssts :: Context SrcSpanInfo -> [Asst SrcSpanInfo]
getAssts = listify (const True :: Asst SrcSpanInfo -> Bool)


-- | Parse mode with all extensions and no fixities.
parseMode :: ParseMode
parseMode = ParseMode
  { parseFilename = ""
  , extensions = glasgowExts
                   ++ [TupleSections, BangPatterns, ViewPatterns]
  , ignoreLinePragmas = False
  , ignoreLanguagePragmas = False
  , fixities = Nothing
  }
