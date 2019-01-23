-- | Operating rate definitions and utilities.
module Sound.SC3.Common.Rate where

import Data.Char {- base -}

-- | Operating rate of unit generator.
--   I = initialisation, K = control, A = audio, D = demand.
--
-- > Data.List.sort [DR,AR,KR,IR] == [IR,KR,AR,DR]
data Rate = IR | KR | AR | DR
            deriving (Eq,Ord,Enum,Bounded,Show,Read)

-- | Integer rate identifier, as required for scsynth bytecode.
rateId :: Rate -> Int
rateId = fromEnum

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
--   I = initialisation, K = control, T = trigger, A = audio.
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
