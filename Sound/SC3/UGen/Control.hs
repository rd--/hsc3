-- | Control
module Sound.SC3.UGen.Control where

import Text.Printf {- base -}

import Sound.SC3.Common.Rate {- hsc3 -}
import Sound.SC3.UGen.Brackets {- hsc3 -}

-- * Control Group

{- | Controls may form part of a control group.
There are presently three types of groups.
Ranges controls have two values (minima, maxima) and are ordinarily drawn as a range slider.
Array controls have n values [e1 .. eN] and are ordinarily drawn as a multislider.
Xy controls have two values (x, y) and are ordinarily drawn as a two dimensional slider.
-}
data Control_Group
  = Control_Range
  | Control_Array Int
  | Control_Xy
  deriving (Ord, Eq, Read, Show)

-- | The number of elements in a control group.
control_group_degree :: Control_Group -> Int
control_group_degree grp =
  case grp of
    Control_Range -> 2
    Control_Array n -> n
    Control_Xy -> 2

{- | Grouped controls have names that have equal prefixes and identifying suffixes.
     Range controls have two elements, minima and maxima, suffixes are [ and ].
     Array controls have n elements and have zero-indexed numerical suffixes.
     Xy controls have two elements, X and Y coordinates, suffixes are X and Y.
-}
control_group_suffixes :: Control_Group -> [String]
control_group_suffixes grp =
  case grp of
    Control_Range -> ["[","]"]
    Control_Array n -> map (printf "%02d") [0 .. n - 1]
    Control_Xy -> ["X","Y"]

-- * Control Meta

-- | Control meta-data.
data Control_Meta n =
    Control_Meta
    {ctl_min :: n -- ^ Minimum
    ,ctl_max :: n -- ^ Maximum
    ,ctl_warp :: String -- ^ @(0,1)@ @(min,max)@ transfer function.
    ,ctl_step :: n -- ^ The step to increment & decrement by.
    ,ctl_units :: String -- ^ Unit of measure (ie hz, ms etc.).
    ,controlGroup :: Maybe Control_Group} -- ^ Control group.
  deriving (Ord, Eq, Read, Show)

-- * Control Meta (T)

-- | 3-tuple form of 'Control_Meta' data.
type Control_Meta_T3 n = (n, n, String)

-- | Lift 'Control_Meta_T3' to 'Control_Meta' allowing type coercion.
control_meta_t3 :: Num m => (n -> m) -> Control_Meta_T3 n -> Control_Meta m
control_meta_t3 f (l,r,w) = Control_Meta (f l) (f r) w 0 "" Nothing

-- | 5-tuple form of 'Control_Meta' data.
type Control_Meta_T5 n = (n,n,String,n,String)

-- | Lift 'Control_Meta_T5' to 'Control_Meta' allowing type coercion.
control_meta_t5 :: (n -> m) -> Control_Meta_T5 n -> Control_Meta m
control_meta_t5 f (l,r,w,stp,u) = Control_Meta (f l) (f r) w (f stp) u Nothing

-- * Control

{- | Control inputs.
It is an unchecked invariant that controls with equal names within a UGen graph must be equal in all other respects.
-}
data Control =
  Control
  {controlOperatingRate :: Rate
  ,controlIndex :: Maybe Int
  ,controlName :: String
  ,controlDefault :: Double
  ,controlTriggered :: Bool
  ,controlMeta :: Maybe (Control_Meta Double)
  ,controlBrackets :: Brackets}
  deriving (Ord, Eq, Read, Show)

{-
-- | Control type
data CVarTy = CVarInit | CVarControl | CVarTrigger deriving (Eq,Read,Show)
CVar CVarTy String Double -- ^ Control input (named)
-}
