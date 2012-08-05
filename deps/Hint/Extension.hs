-- this module was automatically generated. do not edit!
-- edit util/mk_extensions_mod.hs instead
module Hint.Extension (Extension(..),
                       knownExtensions, availableExtensions, asExtension)

where

import Hint.Compat as Compat

-- | List of the extensions known by the interpreter.
availableExtensions :: [Extension]
availableExtensions = map asExtension Compat.supportedExtensions

asExtension :: String -> Extension
asExtension s = if isKnown s
                  then read s
                  else let no_s = "No" ++ s
                  in if isKnown no_s then read no_s
                                     else UnknownExtension s
  where isKnown e = e `elem` map show knownExtensions

-- | This represents language extensions beyond Haskell 98
--   that are supported by GHC (it was taken from
--   Cabal's @Language.Haskell.Extension@)
data Extension = OverlappingInstances
               | UndecidableInstances
               | IncoherentInstances
               | DoRec
               | RecursiveDo
               | ParallelListComp
               | MultiParamTypeClasses
               | NoMonomorphismRestriction
               | FunctionalDependencies
               | Rank2Types
               | RankNTypes
               | PolymorphicComponents
               | ExistentialQuantification
               | ScopedTypeVariables
               | ImplicitParams
               | FlexibleContexts
               | FlexibleInstances
               | EmptyDataDecls
               | CPP
               | KindSignatures
               | BangPatterns
               | TypeSynonymInstances
               | TemplateHaskell
               | ForeignFunctionInterface
               | Arrows
               | Generics
               | NoImplicitPrelude
               | NamedFieldPuns
               | PatternGuards
               | GeneralizedNewtypeDeriving
               | ExtensibleRecords
               | RestrictedTypeSynonyms
               | HereDocuments
               | MagicHash
               | TypeFamilies
               | StandaloneDeriving
               | UnicodeSyntax
               | PatternSignatures
               | UnliftedFFITypes
               | LiberalTypeSynonyms
               | TypeOperators
               | RecordWildCards
               | RecordPuns
               | DisambiguateRecordFields
               | OverloadedStrings
               | GADTs
               | NoMonoPatBinds
               | RelaxedPolyRec
               | ExtendedDefaultRules
               | UnboxedTuples
               | DeriveDataTypeable
               | ConstrainedClassMethods
               | PackageImports
               | ImpredicativeTypes
               | NewQualifiedOperators
               | PostfixOperators
               | QuasiQuotes
               | TransformListComp
               | ViewPatterns
               | XmlSyntax
               | RegularPatterns
               | TupleSections
               | GHCForeignImportPrim
               | NPlusKPatterns
               | DoAndIfThenElse
               | RebindableSyntax
               | ExplicitForAll
               | DatatypeContexts
               | MonoLocalBinds
               | DeriveFunctor
               | DeriveTraversable
               | DeriveFoldable
               | UnknownExtension String
        deriving (Eq, Show, Read)

knownExtensions :: [Extension]
knownExtensions = [OverlappingInstances,
                   UndecidableInstances,
                   IncoherentInstances,
                   DoRec,
                   RecursiveDo,
                   ParallelListComp,
                   MultiParamTypeClasses,
                   NoMonomorphismRestriction,
                   FunctionalDependencies,
                   Rank2Types,
                   RankNTypes,
                   PolymorphicComponents,
                   ExistentialQuantification,
                   ScopedTypeVariables,
                   ImplicitParams,
                   FlexibleContexts,
                   FlexibleInstances,
                   EmptyDataDecls,
                   CPP,
                   KindSignatures,
                   BangPatterns,
                   TypeSynonymInstances,
                   TemplateHaskell,
                   ForeignFunctionInterface,
                   Arrows,
                   Generics,
                   NoImplicitPrelude,
                   NamedFieldPuns,
                   PatternGuards,
                   GeneralizedNewtypeDeriving,
                   ExtensibleRecords,
                   RestrictedTypeSynonyms,
                   HereDocuments,
                   MagicHash,
                   TypeFamilies,
                   StandaloneDeriving,
                   UnicodeSyntax,
                   PatternSignatures,
                   UnliftedFFITypes,
                   LiberalTypeSynonyms,
                   TypeOperators,
                   RecordWildCards,
                   RecordPuns,
                   DisambiguateRecordFields,
                   OverloadedStrings,
                   GADTs,
                   NoMonoPatBinds,
                   RelaxedPolyRec,
                   ExtendedDefaultRules,
                   UnboxedTuples,
                   DeriveDataTypeable,
                   ConstrainedClassMethods,
                   PackageImports,
                   ImpredicativeTypes,
                   NewQualifiedOperators,
                   PostfixOperators,
                   QuasiQuotes,
                   TransformListComp,
                   ViewPatterns,
                   XmlSyntax,
                   RegularPatterns,
                   TupleSections,
                   GHCForeignImportPrim,
                   NPlusKPatterns,
                   DoAndIfThenElse,
                   RebindableSyntax,
                   ExplicitForAll,
                   DatatypeContexts,
                   MonoLocalBinds,
                   DeriveFunctor,
                   DeriveTraversable,
                   DeriveFoldable
                   ]
