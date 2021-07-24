-- | Operating rate definitions and utilities.
module Sound.SC3.Common.Rate where

import Data.Char {- base -}
import Data.Maybe {- base -}

-- | Operating rate of unit generator.
--   I = initialisation, K = control, A = audio, D = demand.
--
-- > Data.List.sort [dr,ar,kr,ir] == [ir,kr,ar,dr]
data Rate = InitialisationRate | ControlRate | AudioRate | DemandRate
            deriving (Eq,Ord,Enum,Bounded,Show,Read)

ir, kr, ar, dr :: Rate
ir = InitialisationRate
kr = ControlRate
ar = AudioRate
dr =DemandRate

{- | Standard SuperCollider rate abbreviations.

> map rateAbbrev [minBound .. maxBound] == ["ir","kr","ar","dr"]
-}
rateAbbrev :: Rate -> String
rateAbbrev rt =
  fromMaybe (error "rateAbbrev?")
  (lookup (fromEnum rt) (zip [0..] (words "ir kr ar dr")))

-- | Integer rate identifier, as required for scsynth bytecode.
rateId :: Rate -> Int
rateId = fromEnum

-- | Color identifiers for each 'Rate'.
rate_color :: Rate -> String
rate_color r =
    case r of
      AudioRate -> "black"
      ControlRate -> "blue"
      InitialisationRate -> "yellow"
      DemandRate -> "red"

-- | Set of all 'Rate' values.
all_rates :: [Rate]
all_rates = [minBound .. maxBound]

-- | Case insensitive parser for rate.
--
-- > Data.Maybe.mapMaybe rate_parse (words "ar kR IR Dr") == [AR,KR,IR,DR]
rate_parse :: String -> Maybe Rate
rate_parse r =
    case map toUpper r of
      "AR" -> Just AudioRate
      "KR" -> Just ControlRate
      "IR" -> Just InitialisationRate
      "DR" -> Just DemandRate
      _ -> Nothing

-- * Control rates

-- | Enumeration of the four operating rates for controls.
--   I = initialisation, K = control, T = trigger, A = audio.
data K_Type = K_InitialisationRate | K_ControlRate | K_TriggerRate | K_AudioRate
             deriving (Eq,Show,Ord)

-- | Determine class of control given 'Rate' and /trigger/ status.
ktype :: Rate -> Bool -> K_Type
ktype r tr =
    if tr
    then case r of
           ControlRate -> K_TriggerRate
           _ -> error "ktype: non ControlRate trigger control"
    else case r of
           InitialisationRate -> K_InitialisationRate
           ControlRate -> K_ControlRate
           AudioRate -> K_AudioRate
           DemandRate -> error "ktype: DemandRate control"
