{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Hint.Compat

where

#if __GLASGOW_HASKELL__ < 702
import Control.Monad.Trans (liftIO)
#endif

#if __GLASGOW_HASKELL__ >= 704
import Control.Monad (foldM, liftM)
#endif

import qualified Hint.GHC as GHC

-- Kinds became a synonym for Type in GHC 6.8. We define this wrapper
-- to be able to define a FromGhcRep instance for both versions
newtype Kind = Kind GHC.Kind

#if __GLASGOW_HASKELL__ >= 700
-- supportedLanguages :: [String]
supportedExtensions = map f GHC.xFlags
    where
#if (__GLASGOW_HASKELL__ < 702) || (__GLASGOW_HASKELL__ >= 704)
      f (e,_,_) = e
#else
      f (e,_,_,_) = e
#endif

#if __GLASGOW_HASKELL__ < 702
-- setContext :: GHC.GhcMonad m => [GHC.Module] -> [GHC.Module] -> m ()
setContext xs = GHC.setContext xs . map (\y -> (y,Nothing))

getContext :: GHC.GhcMonad m => m ([GHC.Module], [GHC.Module])
getContext = fmap (\(as,bs) -> (as,map fst bs)) GHC.getContext
#else
#if __GLASGOW_HASKELL__ < 704
-- Keep setContext/getContext unmodified for use where the results of getContext
-- are simply restored by setContext, in which case we don't really care about the
-- particular type of b.

-- setContext :: GHC.GhcMonad m => [GHC.Module] -> [b] -> m ()
setContext = GHC.setContext

-- getContext :: GHC.GhcMonad m => m ([GHC.Module], [b])
getContext = GHC.getContext
#else
setContext :: GHC.GhcMonad m => [GHC.Module] -> [GHC.ImportDecl GHC.RdrName] -> m ()
setContext ms ds =
  let ms' = map GHC.IIModule ms
      ds' = map GHC.IIDecl ds
      is = ms' ++ ds'
  in GHC.setContext is

getContext :: GHC.GhcMonad m => m ([GHC.Module], [GHC.ImportDecl GHC.RdrName])
getContext = GHC.getContext >>= foldM f ([], [])
  where
    f :: (GHC.GhcMonad m) =>
         ([GHC.Module], [GHC.ImportDecl GHC.RdrName]) ->
         GHC.InteractiveImport ->
         m ([GHC.Module], [GHC.ImportDecl GHC.RdrName])
    f (ns, ds) i = case i of
      (GHC.IIDecl d) -> return (ns, (d:ds))
      (GHC.IIModule n) -> return ((n:ns), ds)
#endif
#endif

mkPState = GHC.mkPState

#else
-- supportedExtensions :: [String]
supportedExtensions = GHC.supportedLanguages

-- setContext :: GHC.GhcMonad m => [GHC.Module] -> [GHC.Module] -> m ()
-- i don't want to check the signature on every ghc version....
setContext = GHC.setContext

getContext = GHC.getContext

mkPState df buf loc = GHC.mkPState buf loc df
#endif

-- Explicitly-typed variants of getContext/setContext, for use where we modify
-- or override the context.
#if __GLASGOW_HASKELL__ < 702
setContextModules :: GHC.GhcMonad m => [GHC.Module] -> [GHC.Module] -> m ()
setContextModules = setContext

getContextNames :: GHC.GhcMonad m => m([String], [String])
getContextNames = fmap (\(as,bs) -> (map name as, map name bs)) getContext
    where name = GHC.moduleNameString . GHC.moduleName
#else
setContextModules :: GHC.GhcMonad m => [GHC.Module] -> [GHC.Module] -> m ()
setContextModules as = setContext as . map (GHC.simpleImportDecl . GHC.moduleName)

getContextNames :: GHC.GhcMonad m => m([String], [String])
getContextNames = fmap (\(as,bs) -> (map name as, map decl bs)) getContext
    where name = GHC.moduleNameString . GHC.moduleName
          decl = GHC.moduleNameString . GHC.unLoc . GHC.ideclName
#endif

#if __GLASGOW_HASKELL__ < 702
mkSrcLoc = GHC.mkSrcLoc
stringToStringBuffer = liftIO . GHC.stringToStringBuffer
#else
mkSrcLoc = GHC.mkRealSrcLoc
stringToStringBuffer = return . GHC.stringToStringBuffer
#endif

#if __GLASGOW_HASKELL__ >= 610
configureDynFlags :: GHC.DynFlags -> GHC.DynFlags
configureDynFlags dflags = dflags{GHC.ghcMode    = GHC.CompManager,
                                  GHC.hscTarget  = GHC.HscInterpreted,
                                  GHC.ghcLink    = GHC.LinkInMemory,
                                  GHC.verbosity  = 0}

parseDynamicFlags :: GHC.GhcMonad m
                   => GHC.DynFlags -> [String] -> m (GHC.DynFlags, [String])
parseDynamicFlags d = fmap firstTwo . GHC.parseDynamicFlags d . map GHC.noLoc
    where firstTwo (a,b,_) = (a, map GHC.unLoc b)

fileTarget :: FilePath -> GHC.Target
fileTarget f = GHC.Target (GHC.TargetFile f $ Just next_phase) True Nothing
    where next_phase = GHC.Cpp GHC.HsSrcFile

targetId :: GHC.Target -> GHC.TargetId
targetId = GHC.targetId

guessTarget :: GHC.GhcMonad m => String -> Maybe GHC.Phase -> m GHC.Target
guessTarget = GHC.guessTarget

-- add a bogus Maybe, in order to use it with mayFail
compileExpr :: GHC.GhcMonad m => String -> m (Maybe GHC.HValue)
compileExpr = fmap Just . GHC.compileExpr

-- add a bogus Maybe, in order to use it with mayFail
exprType :: GHC.GhcMonad m => String -> m (Maybe GHC.Type)
exprType = fmap Just . GHC.exprType

-- add a bogus Maybe, in order to use it with mayFail
#if __GLASGOW_HASKELL__ < 704
typeKind :: GHC.GhcMonad m => String -> m (Maybe GHC.Kind)
typeKind = fmap Just . GHC.typeKind
#else
typeKind :: GHC.GhcMonad m => String -> m (Maybe GHC.Kind)
typeKind = fmap Just . (liftM snd) . (GHC.typeKind True)
#endif
#else
-- add a bogus session parameter, in order to use it with runGhc2
parseDynamicFlags :: GHC.Session
                  -> GHC.DynFlags
                  -> [String] -> IO (GHC.DynFlags, [String])
parseDynamicFlags = const GHC.parseDynamicFlags

fileTarget :: FilePath -> GHC.Target
fileTarget f = GHC.Target (GHC.TargetFile f $ Just next_phase) Nothing
    where next_phase = GHC.Cpp GHC.HsSrcFile

targetId :: GHC.Target -> GHC.TargetId
targetId (GHC.Target _id _) = _id

-- add a bogus session parameter, in order to use it with runGhc2
guessTarget :: GHC.Session -> String -> Maybe GHC.Phase -> IO GHC.Target
guessTarget = const GHC.guessTarget

compileExpr :: GHC.Session -> String -> IO (Maybe GHC.HValue)
compileExpr = GHC.compileExpr

exprType :: GHC.Session -> String -> IO (Maybe GHC.Type)
exprType = GHC.exprType

typeKind :: GHC.Session -> String -> IO (Maybe GHC.Kind)
typeKind = GHC.typeKind

#endif

#if __GLASGOW_HASKELL__ >= 608
#if __GLASGOW_HASKELL__ < 610
  -- 6.08 only
newSession :: FilePath -> IO GHC.Session
newSession ghc_root = GHC.newSession (Just ghc_root)

configureDynFlags :: GHC.DynFlags -> GHC.DynFlags
configureDynFlags dflags = dflags{GHC.ghcMode    = GHC.CompManager,
                                  GHC.hscTarget  = GHC.HscInterpreted,
                                  GHC.ghcLink    = GHC.LinkInMemory}
#endif

#if __GLASGOW_HASKELL__ < 701
  -- 6.08 - 7.0.4
pprType :: GHC.Type -> (GHC.PprStyle -> GHC.Doc)
pprType = GHC.pprTypeForUser False -- False means drop explicit foralls

pprKind :: GHC.Kind -> (GHC.PprStyle -> GHC.Doc)
pprKind = pprType
#else
  -- 7.2.1 and above
pprType :: GHC.Type -> GHC.SDoc
pprType = GHC.pprTypeForUser False -- False means drop explicit foralls

pprKind :: GHC.Kind -> GHC.SDoc
pprKind = pprType
#endif

#elif __GLASGOW_HASKELL__ >= 606
  -- 6.6 only

newSession :: FilePath -> IO GHC.Session
newSession ghc_root = GHC.newSession GHC.Interactive (Just ghc_root)

configureDynFlags :: GHC.DynFlags -> GHC.DynFlags
configureDynFlags dflags = dflags{GHC.hscTarget  = GHC.HscInterpreted}

pprType :: GHC.Type -> (GHC.PprStyle -> GHC.Doc)
pprType = GHC.ppr . GHC.dropForAlls

pprKind :: GHC.Kind -> (GHC.PprStyle -> GHC.Doc)
pprKind = GHC.ppr

#endif

