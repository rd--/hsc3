-- | For hand-writing UGens.
module Sound.SC3.UGen.UGen.Construct where

import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | Oscillator constructor with constrained set of operating 'Rate's.
mk_osc :: [Rate] -> UGenId -> Rate -> String -> [UGen] -> Int -> UGen
mk_osc rs z r c i o =
    if r `elem` rs
    then mkUGen Nothing rs (Left r) c i Nothing o (Special 0) z
    else error ("mk_osc: rate restricted: " ++ show (r, rs, c))

-- | Oscillator constructor with 'all_rates'.
mkOsc :: Rate -> String -> [UGen] -> Int -> UGen
mkOsc = mk_osc all_rates no_id

-- | Oscillator constructor, rate restricted variant.
mkOscR :: [Rate] -> Rate -> String -> [UGen] -> Int -> UGen
mkOscR rs = mk_osc rs no_id

-- | Rate restricted oscillator constructor, setting identifier.
mkOscIdR :: [Rate] -> UGenId -> Rate -> String -> [UGen] -> Int -> UGen
mkOscIdR rr z = mk_osc rr z

-- | Oscillator constructor, setting identifier.
mkOscId :: UGenId -> Rate -> String -> [UGen] -> Int -> UGen
mkOscId z = mk_osc all_rates z

-- | Provided 'UGenId' variant of 'mkOscMCE'.
mk_osc_mce :: UGenId -> Rate -> String -> [UGen] -> UGen -> Int -> UGen
mk_osc_mce z r c i j =
    let i' = i ++ mceChannels j
    in mk_osc all_rates z r c i'

-- | Variant oscillator constructor with MCE collapsing input.
mkOscMCE :: Rate -> String -> [UGen] -> UGen -> Int -> UGen
mkOscMCE = mk_osc_mce no_id

-- | Variant oscillator constructor with MCE collapsing input.
mkOscMCEId :: UGenId -> Rate -> String -> [UGen] -> UGen -> Int -> UGen
mkOscMCEId z = mk_osc_mce z

-- | Rate constrained filter 'UGen' constructor.
mk_filter :: [Rate] -> [Int] -> UGenId -> String -> [UGen] -> Int -> UGen
mk_filter rs ix z c i o = mkUGen Nothing rs (Right ix) c i Nothing o (Special 0) z

-- | Filter UGen constructor.
mkFilterIdR :: [Rate] -> UGenId -> String -> [UGen] -> Int -> UGen
mkFilterIdR rs z nm i o = mk_filter rs [0 .. length i - 1] z nm i o

-- | Filter UGen constructor.
mkFilterR :: [Rate] -> String -> [UGen] -> Int -> UGen
mkFilterR rs = mkFilterIdR rs no_id

-- | Filter 'UGen' constructor.
mkFilter :: String -> [UGen] -> Int -> UGen
mkFilter = mkFilterR all_rates

-- | Filter UGen constructor.
mkFilterId :: UGenId -> String -> [UGen] -> Int -> UGen
mkFilterId = mkFilterIdR all_rates

-- | Provided 'UGenId' filter with 'mce' input.
mk_filter_mce :: [Rate] -> UGenId -> String -> [UGen] -> UGen -> Int -> UGen
mk_filter_mce rs z c i j = mkFilterIdR rs z c (i ++ mceChannels j)

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCER :: [Rate] -> String -> [UGen] -> UGen -> Int -> UGen
mkFilterMCER rs = mk_filter_mce rs no_id

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCE :: String -> [UGen] -> UGen -> Int -> UGen
mkFilterMCE = mk_filter_mce all_rates no_id

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCEId :: UGenId -> String -> [UGen] -> UGen -> Int -> UGen
mkFilterMCEId z = mk_filter_mce all_rates z

-- | Information unit generators are very specialized.
mkInfo :: String -> UGen
mkInfo name = mkOsc IR name [] 1
