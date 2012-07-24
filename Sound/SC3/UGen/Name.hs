-- | Functions to normalise UGen names.
module Sound.SC3.UGen.Name where

import Data.Char

-- | Convert from @hsc3@ name to @SC3@ name.
--
-- > toSC3Name "sinOsc" == "SinOsc"
-- > toSC3Name "lfSaw" == "LFSaw"
-- > toSC3Name "pv_Copy" == "PV_Copy"
toSC3Name :: String -> String
toSC3Name nm =
    case nm of
      'l':'f':nm' -> "LF"++nm'
      'p':'v':'_':nm' -> "PV_"++nm'
      p:q -> toUpper p : q
      [] -> []

-- | Inverse of 'toSC3Name'.
--
-- > let nm = ["SinOsc","LFSaw","PV_Copy"]
-- > in map fromSC3Name nm == ["sinOsc","lfSaw","pv_Copy"]
fromSC3Name :: String -> String
fromSC3Name nm =
    case nm of
      'L':'F':nm' -> "lf"++nm'
      'P':'V':'_':nm' -> "pv_"++nm'
      p:q -> toLower p : q
      [] -> []

