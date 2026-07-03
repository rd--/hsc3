-- | Nrt statistics.
module Sound.Sc3.Server.Nrt.Stat where

import qualified Sound.Osc.Datum as Datum {- hosc -}
import qualified Sound.Osc.Packet as Packet {- hosc -}

import qualified Sound.Sc3.Common.Base as Base {- hsc3 -}
import Sound.Sc3.Server.Nrt {- hsc3 -}

-- | Nrt statistics, see nrt_stat_param for meanings.
type Nrt_Stat =
  ( (String, Datum.Time)
  , (String, Int)
  , (String, Int)
  , (String, [(String, Int)])
  )

-- | Nrt_Stat names.
nrt_stat_param :: (String, String, String, String)
nrt_stat_param = ("duration", "# bundles", "# messages", "command set")

-- | Trivial Nrt statistics.
nrt_stat :: Nrt -> Nrt_Stat
nrt_stat (Nrt b_seq) =
  let b_msg = map Packet.bundleMessages b_seq
  in Base.p4_zip
      nrt_stat_param
      ( Packet.bundleTime (last b_seq)
      , length b_seq
      , sum (map length b_msg)
      , Base.histogram (concatMap (map Packet.messageAddress) b_msg)
      )
