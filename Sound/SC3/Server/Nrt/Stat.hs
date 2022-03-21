-- | Nrt statistics.
module Sound.SC3.Server.Nrt.Stat where

import Sound.Osc.Datum {- hosc -}
import Sound.Osc.Packet {- hosc -}

import Sound.SC3.Common.Base {- hsc3 -}
import Sound.SC3.Server.Nrt {- hsc3 -}

-- | Nrt statistics, see nrt_stat_param for meanings.
type Nrt_Stat =
    ((String, Time)
    ,(String, Int)
    ,(String, Int)
    ,(String, [(String,Int)]))

-- | Nrt_Stat names.
nrt_stat_param :: (String, String, String, String)
nrt_stat_param = ("duration","# bundles","# messages","command set")

-- | Trivial Nrt statistics.
nrt_stat :: Nrt -> Nrt_Stat
nrt_stat (Nrt b_seq) =
    let b_msg = map bundleMessages b_seq
    in p4_zip
       nrt_stat_param
       (bundleTime (last b_seq)
       ,length b_seq
       ,sum (map length b_msg)
       ,histogram (concatMap (map messageAddress) b_msg))
