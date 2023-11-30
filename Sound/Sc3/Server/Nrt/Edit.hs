-- | Nrt editing
module Sound.Sc3.Server.Nrt.Edit where

import Data.List {- base -}

import qualified Data.List.Ordered {- data-ordlist -}

import qualified Sound.Osc.Datum as Osc {- hosc -}
import qualified Sound.Osc.Packet as Osc {- hosc -}

import qualified Sound.Sc3.Common.Base {- hsc3 -}
import qualified Sound.Sc3.Server.Command {- hsc3 -}
import Sound.Sc3.Server.Nrt {- hsc3 -}

-- | Merge two Nrt scores.  Retains internal nrt_end messages.
nrt_merge :: Nrt -> Nrt -> Nrt
nrt_merge (Nrt p) (Nrt q) = Nrt (Data.List.Ordered.merge p q)

-- | Merge a set of Nrt.  Retains internal 'nrt_end' messages.
nrt_merge_set :: [Nrt] -> Nrt
nrt_merge_set = foldr nrt_merge nrt_empty

-- | The empty Nrt.
nrt_empty :: Nrt
nrt_empty = Nrt []

-- | Add bundle at first permissable location of Nrt.
nrt_insert_pre :: Osc.BundleOf Osc.Message -> Nrt -> Nrt
nrt_insert_pre p (Nrt q) = Nrt (insert p q)

-- | Add bundle at last permissable location of Nrt.
nrt_insert_post :: Osc.BundleOf Osc.Message -> Nrt -> Nrt
nrt_insert_post p (Nrt q) = Nrt (Sound.Sc3.Common.Base.insert_post p q)

-- | bundleTime of last of nrt_bundles.
nrt_end_time :: Nrt -> Osc.Time
nrt_end_time = Osc.bundleTime . last . nrt_bundles

-- | Delete any internal nrt_end messages, and require one at the final bundle.
nrt_close :: Nrt -> Nrt
nrt_close (Nrt l) =
    let is_nrt_end_msg = (== "/nrt_end") . Osc.messageAddress
        bundle_map t_f m_f (Osc.Bundle t m) = Osc.Bundle (t_f t) (m_f m) -- apply temporal and message functions to bundle
        rem_end_msg = bundle_map id (filter (not . is_nrt_end_msg))
        req_end_msg = let f m = if any is_nrt_end_msg m
                                then m
                                else m ++ [Sound.Sc3.Server.Command.nrt_end]
                      in bundle_map id f
    in Nrt (Sound.Sc3.Common.Base.at_last rem_end_msg req_end_msg l)

-- | Append /q/ to /p/, assumes last timestamp at /p/ precedes first at /q/.
nrt_append :: Nrt -> Nrt -> Nrt
nrt_append (Nrt p) (Nrt q) = Nrt (p ++ q)
