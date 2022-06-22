-- | For hand-writing Ugens.
module Sound.Sc3.Ugen.Bindings.Hw.Construct where

import Sound.Sc3.Common.Rate

import Sound.Sc3.Ugen.Types

-- | Oscillator constructor with constrained set of operating 'Rate's.
mk_osc :: [Rate] -> UgenId -> Rate -> String -> [Ugen] -> Int -> Ugen
mk_osc rs z r c i o =
    if r `elem` rs
    then mkUgen Nothing rs (Left r) c i Nothing o (Special 0) z
    else error ("mk_osc: rate restricted: " ++ show (r, rs, c))

-- | Oscillator constructor with 'all_rates'.
mkOsc :: Rate -> String -> [Ugen] -> Int -> Ugen
mkOsc = mk_osc all_rates no_id

-- | Oscillator constructor, rate restricted variant.
mkOscR :: [Rate] -> Rate -> String -> [Ugen] -> Int -> Ugen
mkOscR rs = mk_osc rs no_id

-- | Rate restricted oscillator constructor, setting identifier.
mkOscIdR :: [Rate] -> UgenId -> Rate -> String -> [Ugen] -> Int -> Ugen
mkOscIdR = mk_osc

-- | Oscillator constructor, setting identifier.
mkOscId :: UgenId -> Rate -> String -> [Ugen] -> Int -> Ugen
mkOscId = mk_osc all_rates

-- | Provided 'UgenId' variant of 'mkOscMCE'.
mk_osc_mce :: UgenId -> Rate -> String -> [Ugen] -> Ugen -> Int -> Ugen
mk_osc_mce z r c i j =
    let i' = i ++ mceChannels j
    in mk_osc all_rates z r c i'

-- | Variant oscillator constructor with MCE collapsing input.
mkOscMCE :: Rate -> String -> [Ugen] -> Ugen -> Int -> Ugen
mkOscMCE = mk_osc_mce no_id

-- | Variant oscillator constructor with MCE collapsing input.
mkOscMCEId :: UgenId -> Rate -> String -> [Ugen] -> Ugen -> Int -> Ugen
mkOscMCEId = mk_osc_mce

-- | Rate constrained filter 'Ugen' constructor.
mk_filter :: [Rate] -> [Int] -> UgenId -> String -> [Ugen] -> Int -> Ugen
mk_filter rs ix z c i o = mkUgen Nothing rs (Right ix) c i Nothing o (Special 0) z

-- | Filter Ugen constructor.
mkFilterIdR :: [Rate] -> UgenId -> String -> [Ugen] -> Int -> Ugen
mkFilterIdR rs z nm i = mk_filter rs [0 .. length i - 1] z nm i

-- | Filter Ugen constructor.
mkFilterR :: [Rate] -> String -> [Ugen] -> Int -> Ugen
mkFilterR rs = mkFilterIdR rs no_id

-- | Filter 'Ugen' constructor.
mkFilter :: String -> [Ugen] -> Int -> Ugen
mkFilter = mkFilterR all_rates

-- | Filter Ugen constructor.
mkFilterId :: UgenId -> String -> [Ugen] -> Int -> Ugen
mkFilterId = mkFilterIdR all_rates

-- | Provided 'UgenId' filter with 'mce' input.
mk_filter_mce :: [Rate] -> UgenId -> String -> [Ugen] -> Ugen -> Int -> Ugen
mk_filter_mce rs z c i j = mkFilterIdR rs z c (i ++ mceChannels j)

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCER :: [Rate] -> String -> [Ugen] -> Ugen -> Int -> Ugen
mkFilterMCER rs = mk_filter_mce rs no_id

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCE :: String -> [Ugen] -> Ugen -> Int -> Ugen
mkFilterMCE = mk_filter_mce all_rates no_id

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCEId :: UgenId -> String -> [Ugen] -> Ugen -> Int -> Ugen
mkFilterMCEId = mk_filter_mce all_rates

-- | Information unit generators are very specialized.
mkInfo :: String -> Ugen
mkInfo name = mkOsc InitialisationRate name [] 1
