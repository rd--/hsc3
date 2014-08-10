-- | Functions to normalise UGen names.  @SC3@ UGen names are
-- capitalised, @hsc3@ cannot use the same names for UGen constructor
-- functions.  The functions here are heuristics, and are likely only
-- partial.
module Sound.SC3.UGen.Name where

import Data.Char {- base -}
import Data.List.Split {- split -}

import Sound.SC3.UGen.Rate {- hsc3 -}

-- | Convert from @hsc3@ name to @SC3@ name.
--
-- > toSC3Name "sinOsc" == "SinOsc"
-- > toSC3Name "lfSaw" == "LFSaw"
-- > toSC3Name "pv_Copy" == "PV_Copy"
-- > map toSC3Name ["bpf","fft","tpv","out","in'"]
toSC3Name :: String -> String
toSC3Name nm =
    case nm of
      "in'" -> "In"
      "bpz2" -> "BPZ2"
      "brz2" -> "BRZ2"
      "hpz1" -> "HPZ1"
      "ifft" -> "IFFT"
      "lpz1" -> "LPZ1"
      "out" -> "Out"
      "rhpf" -> "RHPF"
      "rlpf" -> "RLPF"
      'l':'f':'d':nm' -> "LFD" ++ nm'
      'l':'p':'z':nm' -> "LPZ" ++ nm'
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
--
-- > map fromSC3Name (words "HPZ1 RLPF")
fromSC3Name :: String -> String
fromSC3Name nm =
    case nm of
      "In" -> "in'"
      "BPZ2" -> "bpz2"
      "BRZ2" -> "brz2"
      "HPZ1" -> "hpz1"
      "IFFT" -> "ifft"
      "LPZ1" -> "lpz1"
      "RHPF" -> "rhpf"
      "RLPF" -> "rlpf"
      'L':'F':'D':nm' -> "lfd" ++ nm'
      'l':'p':'z':nm' -> "lpz" ++ nm'
      'L':'F':nm' -> "lf" ++ nm'
      'P':'V':'_':nm' -> "pv_" ++ nm'
      p:q -> if all isUpper nm && length nm <= 3
             then map toLower nm
             else toLower p : q
      [] -> []

-- | (prev,cur,next) triples.
--
-- > pcn_triples [1..3] == [(Nothing,1,Just 2),(Just 1,2,Just 3),(Just 2,3,Nothing)]
pcn_triples :: [a] -> [(Maybe a,a,Maybe a)]
pcn_triples =
    let f e l = case l of
                  e1 : e2 : l' -> (e,e1,Just e2) : f (Just e1) (e2 : l')
                  [e'] -> [(e,e',Nothing)]
                  [] -> undefined
    in f Nothing

-- | Find SC3 name edges.
sc3_name_edges :: String -> [Bool]
sc3_name_edges =
    let f t = case t of
                (Nothing,_,_) -> False
                (Just p,q,Just r) ->
                    (isLower p && isUpper q) ||
                    (isUpper p && isUpper q && isLower r && (p /= 'U' && q /= 'G'))
                (Just p,q,Nothing) -> isLower p && isUpper q
    in map f . pcn_triples

-- | Convert from SC3 name to Lisp style name.
--
-- > let {s = words "SinOsc LFSaw FFT PV_Add AllpassN BHiPass BinaryOpUGen HPZ1 RLPF"
-- >     ;l = ["sin-osc","lf-saw","fft","pv-add","allpass-n","b-hi-pass","binary-op-ugen","hpz1","rlpf"]}
-- > in map sc3_name_to_lisp_name s == l
sc3_name_to_lisp_name :: String -> String
sc3_name_to_lisp_name s =
    let f (c,e) = if e then ['-',c] else if c == '_' then "-" else [c]
    in concatMap f (zip (map toLower s) (sc3_name_edges s))

-- | SC3 UGen /names/ are given with rate suffixes if oscillators, without if filters.
--
-- > map sc3_ugen_name_sep (words "SinOsc.ar LPF *")
sc3_ugen_name_sep :: String -> Maybe (String,Maybe Rate)
sc3_ugen_name_sep u =
    case splitOn "." u of
      [nm,rt] -> Just (nm,rate_parse (map toUpper rt))
      [nm] -> Just (nm,Nothing)
      _ -> Nothing
