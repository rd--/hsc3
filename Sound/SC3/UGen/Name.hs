-- | Functions to normalise UGen names.  @SC3@ UGen names are
-- capitalised, @hsc3@ cannot use the same names for UGen constructor
-- functions.  The functions here are heuristics, and are likely only
-- partial.
module Sound.SC3.UGen.Name where

import Data.Char {- base -}

-- | Convert from @hsc3@ name to @SC3@ name.
--
-- > toSC3Name "sinOsc" == "SinOsc"
-- > toSC3Name "lfSaw" == "LFSaw"
-- > toSC3Name "pv_Copy" == "PV_Copy"
-- > map toSC3Name ["bpf","fft","tpv"] == ["BPF","FFT","TPV"]
toSC3Name :: String -> String
toSC3Name nm =
    case nm of
      'l':'f':nm' -> "LF" ++ nm'
      'p':'v':'_':nm' -> "PV_" ++ nm'
      p:q -> if all isLower nm && length nm <= 3
             then map toUpper nm
             else toUpper p : q
      [] -> []

-- | Inverse of 'toSC3Name'.
--
-- > let nm = ["SinOsc","LFSaw","PV_Copy"]
-- > in map fromSC3Name nm == ["sinOsc","lfSaw","pv_Copy"]
--
-- > map fromSC3Name ["BPF","FFT","TPV"] == ["bpf","fft","tpv"]
fromSC3Name :: String -> String
fromSC3Name nm =
    case nm of
      'L':'F':nm' -> "lf"++nm'
      'P':'V':'_':nm' -> "pv_"++nm'
      p:q -> if all isUpper nm && length nm <= 3
             then map toLower nm
             else toLower p : q
      [] -> []

