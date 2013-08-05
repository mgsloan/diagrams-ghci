{-# LANGUAGE
    MultiParamTypeClasses
  , DefaultSignatures
  , FlexibleInstances
  , TypeSynonymInstances
  , OverlappingInstances
  #-}

-- |Used as the context for diagrams-ghci
module MyPrelude () where
import Prelude
import Data.List     (intersperse)
import Data.Ratio
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.TwoD.Path.Turtle
import Diagrams.TwoD.Text
import Graphics.UI.Toy.Gtk.Prelude

class GhcdiShow a where
  -- First parameter is animation time
  ghcdiShow :: a -> Double -> CairoDiagram

  default ghcdiShow :: Show a => a -> Double -> CairoDiagram
  ghcdiShow x _ = preText $ show x

-- You'd better be using cairo diagrams... :)
instance GhcdiShow CairoDiagram where
  ghcdiShow d _ = d

listLike' :: String -> String -> String
          -> [CairoDiagram] -> CairoDiagram
listLike' s m e = listLike (preText s) (preText m) (preText e)

listLike :: CairoDiagram -> CairoDiagram -> CairoDiagram
         -> [CairoDiagram] -> CairoDiagram
listLike s m e xs =  centerY (scale_it s) ||| inner ||| centerY (scale_it e)
  where
    inner = hcat . intersperse (m # translateY (max_height / (-2))) $ map centerY xs
    max_height = maximum $ map height xs
    ratio = max_height / height s
    scale_it = scaleX (1 - (1 - ratio) / 5) . scaleY (ratio * 1.2)

instance GhcdiShow a => GhcdiShow [a] where
  ghcdiShow xs t = listLike' "[" ", " "]" $ map (`ghcdiShow` t) xs

instance (GhcdiShow a, GhcdiShow b) => GhcdiShow (a, b) where
  ghcdiShow (x, y)       t
    = listLike' "(" ", " ")" [ghcdiShow x t, ghcdiShow y t]

instance (GhcdiShow a, GhcdiShow b, GhcdiShow c)
      => GhcdiShow (a, b, c) where
  ghcdiShow (x, y, u)    t
    = listLike' "(" ", " ")" [ghcdiShow x t, ghcdiShow y t, ghcdiShow u t]

instance (GhcdiShow a, GhcdiShow b, GhcdiShow c, GhcdiShow d)
      => GhcdiShow (a, b, c, d) where
  ghcdiShow (x, y, u, v) t
    = listLike' "(" ", " ")" [ghcdiShow x t, ghcdiShow y t, ghcdiShow u t, ghcdiShow v t]

ctxt = centerY . preText
cgs x = centerY . ghcdiShow x

instance GhcdiShow a => GhcdiShow (Maybe a) where
  ghcdiShow Nothing  _ = ctxt "Nothing"
  ghcdiShow (Just x) t = ctxt "Just " ||| cgs x t

instance (GhcdiShow a, GhcdiShow b) => GhcdiShow (Either a b) where
  ghcdiShow (Left  x) t = ctxt "Right " ||| cgs x t
  ghcdiShow (Right x) t = ctxt "Left "  ||| cgs x t

instance (GhcdiShow a, Integral a) => GhcdiShow (Ratio a) where
  ghcdiShow r t = centerX above === centerX (hrule bar_width) === centerX below
    where
      bar_width = max (width above) (width below)
      above = ghcdiShow (numerator   r) t
      below = ghcdiShow (denominator r) t

instance GhcdiShow Int where
instance GhcdiShow Integer where
instance GhcdiShow Rational where
instance GhcdiShow Float where
instance GhcdiShow Char where
instance GhcdiShow Double where
instance GhcdiShow Bool where
instance GhcdiShow String where
