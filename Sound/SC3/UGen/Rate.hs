-- | Operating rate definitions and utilities.
module Sound.SC3.UGen.Rate where

import Data.Char {- base -}
import Data.Function {- base -}

-- | Operating rate of unit generator.
data Rate = IR | KR | AR | DR
            deriving (Eq,Enum,Bounded,Show,Read)

instance Ord Rate where
    compare = compare `on` rate_ord

-- | Integer rate identifier, as required for scsynth bytecode.
rateId :: Rate -> Int
rateId = fromEnum

-- | Rates as ordered for filter rate selection.
rate_ord :: Rate -> Int
rate_ord r =
    case r of
      IR -> 0
      KR -> 1
      AR -> 2
      DR -> 3 -- ?

-- | Color identifiers for each 'Rate'.
rate_color :: Rate -> String
rate_color r =
    case r of
      AR -> "black"
      KR -> "blue"
      IR -> "yellow"
      DR -> "red"

-- | Set of all 'Rate' values.
all_rates :: [Rate]
all_rates = [minBound .. maxBound]

-- | Case insensitive parser for rate.
--
-- > Data.Maybe.mapMaybe rate_parse (words "ar kR IR Dr") == [AR,KR,IR,DR]
rate_parse :: String -> Maybe Rate
rate_parse r =
    case map toUpper r of
      "AR" -> Just AR
      "KR" -> Just KR
      "IR" -> Just IR
      "DR" -> Just DR
      _ -> Nothing

-- * Control rates

-- | Enumeration of the four operating rates for controls.
--   IR = initialisation rate, KR = control rate, TR = trigger rate, AR = audio rate.
data K_Type = K_IR | K_KR | K_TR | K_AR
             deriving (Eq,Show,Ord)

-- | Determine class of control given 'Rate' and /trigger/ status.
ktype :: Rate -> Bool -> K_Type
ktype r tr =
    if tr
    then case r of
           KR -> K_TR
           _ -> error "ktype: non KR trigger control"
    else case r of
           IR -> K_IR
           KR -> K_KR
           AR -> K_AR
           DR -> error "ktype: DR control"
