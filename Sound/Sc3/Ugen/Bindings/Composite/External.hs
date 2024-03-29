-- | Common unit generator graphs.
module Sound.Sc3.Ugen.Bindings.Composite.External where

import Data.List {- base -}
import Data.Maybe {- base -}

import Sound.Sc3.Common.Math {- hsc3 -}
import Sound.Sc3.Common.Rate {- hsc3 -}
import Sound.Sc3.Common.Uid {- hsc3 -}
import Sound.Sc3.Common.Unsafe {- hsc3 -}

import Sound.Sc3.Ugen.Analysis {- hsc3 -}
import Sound.Sc3.Ugen.Ugen {- hsc3 -}
import Sound.Sc3.Ugen.Util {- hsc3 -}

import qualified Sound.Sc3.Ugen.Bindings.Db as Db {- hsc3 -}
import qualified Sound.Sc3.Ugen.Bindings.Db.External as External {- hsc3 -}

-- | FM7 variant where input matrices are not in Mce form.
fm7_mx :: [[Ugen]] -> [[Ugen]] -> Ugen
fm7_mx ctlMatrix modMatrix = External.fm7 ar (mce (concat ctlMatrix)) (mce (concat modMatrix))

{- | greyhole re-orders parameters as well as unpacking the input signal.

in1=0.0 in2=0.0 damping=0.0 delayTime=2.0 diffusion=0.5 feedback=0.9 moddepth=0.1 modfreq=2.0 size=1.0
in              delayTime=2.0 damping=0.0 size=1.0 diffusion=0.7 feedback=0.9 modDepth=0.1 modFreq=2.0
-}
greyhole :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
greyhole i delayTime damping size diffusion feedback modDepth modFreq =
  let (i1, i2) = unmce2 i
  in External.greyholeRaw i1 i2 damping delayTime diffusion feedback modDepth modFreq size

-- | Association list giving names for MiBraids modes.
miBraids_mode_dict :: Num n => [(n, String)]
miBraids_mode_dict =
  [ (0, "CSAW")
  , (1, "MORPH")
  , (2, "SAW_SQUARE")
  , (3, "SINE_TRIANGLE")
  , (4, "BUZZ")
  , (5, "SQUARE_SUB")
  , (6, "SAW_SUB")
  , (7, "SQUARE_SYNC")
  , (8, "SAW_SYNC")
  , (9, "TRIPLE_SAW")
  , (10, "TRIPLE_SQUARE")
  , (11, "TRIPLE_TRIANGLE")
  , (12, "TRIPLE_SINE")
  , (13, "TRIPLE_RING_MOD")
  , (14, "SAW_SWARM")
  , (15, "SAW_COMB")
  , (16, "TOY")
  , (17, "DIGITAL_FILTER_LP")
  , (18, "DIGITAL_FILTER_PK")
  , (19, "DIGITAL_FILTER_BP")
  , (20, "DIGITAL_FILTER_HP")
  , (21, "VOSIM")
  , (22, "VOWEL")
  , (23, "VOWEL_FOF")
  , (24, "HARMONICS")
  , (25, "FM")
  , (26, "FEEDbACK_FM")
  , (27, "CHAOTIC_FEEDbACK_FM")
  , (28, "PLUCKED")
  , (29, "BOWED")
  , (30, "BLOWN")
  , (31, "FLUTED")
  , (32, "STRUCK_BELL")
  , (33, "STRUCK_DRUM")
  , (34, "KICK")
  , (35, "CYMBAL")
  , (36, "SNARE")
  , (37, "WAVETABLES")
  , (38, "WAVE_MAP")
  , (39, "WAVE_LINE")
  , (40, "WAVE_PARAPHONIC")
  , (41, "FILTERED_NOISE")
  , (42, "TWIN_PEAKS_NOISE")
  , (43, "CLOCKED_NOISE")
  , (44, "GRANULAR_CLOUD")
  , (45, "PARTICLE_NOISE")
  , (46, "DIGITAL_MODULATION")
  , (47, "QUESTION_MARK")
  ]

-- | Reverse lookup of 'miBraids_mode_dict'.
miBraids_mode_maybe :: Num n => String -> Maybe n
miBraids_mode_maybe x = fmap fst (find ((== x) . snd) miBraids_mode_dict)

-- | 'error' of 'miBraids_mode_maybe'
miBraids_mode :: Num n => String -> n
miBraids_mode = fromMaybe (error "miBraids_mode?") . miBraids_mode_maybe

-- | Association list giving names for MiClouds modes.
miClouds_mode_dict :: Num n => [(n, String)]
miClouds_mode_dict =
  [ (0, "GRANULAR")
  , (1, "STRETCH")
  , (2, "LOOPING_DELAY")
  , (3, "SPECTRAL")
  ]

-- | Reverse lookup of 'miClouds_mode_dict'.
miClouds_mode_maybe :: Num n => String -> Maybe n
miClouds_mode_maybe x = fmap fst (find ((== x) . snd) miClouds_mode_dict)

-- | 'error' of 'miClouds_mode_maybe'
miClouds_mode :: Num n => String -> n
miClouds_mode = fromMaybe (error "miClouds_mode?") . miClouds_mode_maybe

-- | Association list giving names for MiPlaits modes.
miPlaits_mode_dict :: Num n => [(n, String)]
miPlaits_mode_dict =
  [ (0, "virtual_analog")
  , (1, "waveshaping")
  , (2, "fm")
  , (3, "grain")
  , (4, "additive")
  , (5, "wavetable")
  , (6, "chord")
  , (7, "speech")
  , (8, "swarm")
  , (9, "noise")
  , (10, "particle")
  , (11, "string")
  , (12, "modal")
  , (13, "bass_drum")
  , (14, "snare_drum")
  , (15, "hi_hat")
  ]

-- | Reverse lookup of 'miPlaits_mode_dict'.
miPlaits_mode_maybe :: Num n => String -> Maybe n
miPlaits_mode_maybe x = fmap fst (find ((== x) . snd) miPlaits_mode_dict)

-- | 'error' of 'miPlaits_mode_maybe'
miPlaits_mode :: Num n => String -> n
miPlaits_mode = fromMaybe (error "miPlaits_mode?") . miPlaits_mode_maybe

-- | Association list giving names for MiPlaits modes.
miRings_mode_dict :: Num n => [(n, String)]
miRings_mode_dict =
  [ (0, "MODAL_RESONATOR")
  , (1, "SYMPATHETIC_STRING")
  , (2, "MODULATED/INHARMONIC_STRING")
  , (3, "2-OP_FM_VOICE")
  , (4, "SYMPATHETIC_STRING_QUANTIZED")
  , (5, "STRING_AND_REVERB")
  ]

-- | Reverse lookup of 'miRings_mode_dict'.
miRings_mode_maybe :: Num n => String -> Maybe n
miRings_mode_maybe x = fmap fst (find ((== x) . snd) miRings_mode_dict)

-- | 'error' of 'miRings_mode_maybe'
miRings_mode :: Num n => String -> n
miRings_mode = fromMaybe (error "miRings_mode?") . miRings_mode_maybe

-- | pulse signal as difference of two 'sawDPW' signals.
pulseDPW :: Rate -> Ugen -> Ugen -> Ugen
pulseDPW rt freq width =
  let o1 = External.sawDPW rt freq 0
      o2 = External.sawDPW rt freq (wrap_hs (-1, 1) (width + width))
  in o1 - o2

-- * rdu

-- | Variant that unpacks the output /mce/ node.
pv_SplitUnpack :: Ugen -> Ugen -> (Ugen, Ugen)
pv_SplitUnpack a b =
  case mceChannels (External.pv_Split a b) of
    [p, q] -> (p, q)
    _ -> error "pv_SplitUnpack"

{- | A 'pv_Split' variant, like 'ffta', that allocates a 'localBuf' by
tracing the input graph to locate the parent @FFT@ or @PV_Split@
node.
-}
pv_SplitAllocId :: ID i => i -> Ugen -> Ugen
pv_SplitAllocId z u =
  case pv_track_nframes u of
    Left err -> error err
    Right nf -> let b = Db.localBufId z nf 1 in External.pv_Split u b

pv_SplitAllocM :: Uid m => Ugen -> m Ugen
pv_SplitAllocM = liftUid1 pv_SplitAllocId

pv_SplitAlloc :: Ugen -> Ugen
pv_SplitAlloc = liftUnsafe1 pv_SplitAllocM

-- | Variant that unpacks the output /mce/ node.
pv_SplitAllocUnpackId :: ID i => i -> Ugen -> (Ugen, Ugen)
pv_SplitAllocUnpackId z u =
  case mceChannels (pv_SplitAllocId z u) of
    [p, q] -> (p, q)
    _ -> error "pv_SplitAllocUnpackId"
