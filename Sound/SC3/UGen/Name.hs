-- | Functions to normalise UGen names.  @SC3@ UGen names are
-- capitalised, @hsc3@ cannot use the same names for UGen constructor
-- functions.  The functions here are heuristics, and are likely only
-- partial.
module Sound.SC3.UGen.Name where

import Data.Char {- base -}
import Data.List.Split {- split -}

import qualified Sound.SC3.Common.Prelude as Common {- hsc3 -}
import Sound.SC3.UGen.Rate {- hsc3 -}

is_uc_or_num :: Char -> Bool
is_uc_or_num c = isUpper c || isDigit c

is_lc_or_num :: Char -> Bool
is_lc_or_num c = isLower c || isDigit c

-- | Find SC3 name edges.
--
-- > sc3_name_edges "SinOsc" == [False,False,False,True,False,False]
-- > sc3_name_edges "DFM1" == [False,False,False,False]
-- > sc3_name_edges "PV_Add" == [False,False,False,True,False,False]
-- > sc3_name_edges "A2K" == [False,False,True]
-- > sc3_name_edges "lag2UD" == [False,False,False,False,True,True]
sc3_name_edges :: String -> [Bool]
sc3_name_edges =
    let f t =
          case t of
            (Nothing,_,_) -> False
            (Just '_',_,_) -> True
            (Just p,q,Just r) -> (is_lc_or_num p && isUpper q) || (isUpper p && isUpper q && is_lc_or_num r)
            (_,q,Nothing) -> isUpper q
    in map f . Common.pcn_triples

-- | Convert from SC3 name to HS style name.
--
-- > s = words "SinOsc LFSaw FFT PV_Add AllpassN BHiPass BinaryOpUGen HPZ1 RLPF TGrains DFM1 FBSineC A2K Lag2UD IIRFilter FMGrainB"
-- > l = words "sinOsc lfSaw fft pv_Add allpassN bHiPass binaryOpUGen hpz1 rlpf tGrains dfm1 fbSineC a2k lag2UD iirFilter"
-- > map sc3_name_to_hs_name s == l
sc3_name_to_hs_name :: String -> String
sc3_name_to_hs_name s =
    let f (c,e) = if e then toUpper c else c
        s_lc = map toLower s
    in if all is_uc_or_num s then s_lc else map f (zip s_lc (sc3_name_edges s))

-- | Convert from SC3 name to Lisp style name.
--
-- > s = words "SinOsc LFSaw FFT PV_Add AllpassN BHiPass BinaryOpUGen HPZ1 RLPF TGrains DFM1"
-- > l = words "sin-osc lf-saw fft pv-add allpass-n b-hi-pass binary-op-u-gen hpz1 rlpf t-grains dfm1"
-- > map sc3_name_to_lisp_name s == l
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
