{- | Functions to normalise UGen names.

@SC3@ UGen names are capitalised.
@hsc3@ cannot use these names for UGen constructor functions.
Haskell names are given by lower-casing until the first word edge.
Lisp names are given by lower-casing everything and adding hyphens before edges.
-}
module Sound.SC3.UGen.Name where

import Data.Char {- base -}
import Data.List.Split {- split -}

import Sound.SC3.Common.Rate {- hsc3 -}

{-
import qualified Sound.SC3.Common.Base {- hsc3 -}

is_uc_or_num :: Char -> Bool
is_uc_or_num c = isUpper c || isDigit c

is_lc_or_num :: Char -> Bool
is_lc_or_num c = isLower c || isDigit c
-}

-- | Find all SC3 name edges. Edges occur at non lower-case letters.
sc3_name_edges_plain :: String -> [Bool]
sc3_name_edges_plain = map (not . isLower)

{- | Find non-initial SC3 name edges.

> sc3_name_edges "SinOsc" == [False,False,False,True,False,False]
> sc3_name_edges "FFT" == [False,False,False]
> sc3_name_edges "DFM1" == [False,False,False,False]
> sc3_name_edges "PV_Add" == [False,False,False,True,False,False]
> sc3_name_edges "A2K" == [False,False,False]
> sc3_name_edges "Lag2UD" == [False,False,False,True,True,True]
-}
sc3_name_edges :: String -> [Bool]
sc3_name_edges s =
  let (p,q) = span (== True) (sc3_name_edges_plain s)
      n = length p
  in if n < 2 || null q
     then replicate n False ++ q
     else replicate (n - 1) False ++ [True] ++ q

{- | Convert from SC3 name to HS style name.

> s = words "SinOsc LFSaw FFT PV_Add AllpassN BHiPass BinaryOpUGen HPZ1 RLPF TGrains DFM1 FBSineC A2K Lag2UD IIRFilter FMGrainB"
> l = words "sinOsc lfSaw fft pv_Add allpassN bHiPass binaryOpUGen hpz1 rlpf tGrains dfm1 fbSineC a2k lag2UD iirFilter fmGrainB"
> map sc3_name_to_hs_name s == l
-}
sc3_name_to_hs_name :: String -> String
sc3_name_to_hs_name s =
    let f (c,e) = if e then toUpper c else c
        s_lc = map toLower s
    in map f (zip s_lc (sc3_name_edges s))

{- | Convert from SC3 name to Lisp style name.

> s = words "SinOsc LFSaw FFT PV_Add AllpassN BHiPass BinaryOpUGen HPZ1 RLPF TGrains DFM1"
> l = words "sin-osc lf-saw fft pv-add allpass-n b-hi-pass binary-op-u-gen hpz1 rlpf t-grains dfm1"
> map sc3_name_to_lisp_name s == l
-}
sc3_name_to_lisp_name :: String -> String
sc3_name_to_lisp_name s =
    let f (c,e) = if e then ['-',c] else if c == '_' then [] else [c]
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
