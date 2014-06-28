-- | 'NRT' operations.
module Sound.SC3.Server.NRT.Edit where

import Data.List {- base -}
import qualified Data.List.Ordered as L {- data-ordlist -}
import Sound.OSC {- hosc -}

import Sound.SC3.Server.Command
import Sound.SC3.Server.NRT

-- * List

-- | Inserts at the first position where it compares less but not
-- equal to the next element.
--
-- > import Data.Function
-- > insertBy (compare `on` fst) (3,'x') (zip [1..5] ['a'..])
-- > insertBy_post (compare `on` fst) (3,'x') (zip [1..5] ['a'..])
insertBy_post :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy_post cmp e l =
    case l of
      [] -> [e]
      h:l' -> case cmp e h of
                LT -> e : l
                _ -> h : insertBy_post cmp e l'

-- | 'insertBy_post' using 'compare'.
insert_post :: Bundle -> [Bundle] -> [Bundle]
insert_post = insertBy_post compare

-- | Apply /f/ at all but last element, and /g/ at last element.
--
-- > at_last (* 2) negate [1..4] == [2,4,6,-4]
at_last :: (a -> b) -> (a -> b) -> [a] -> [b]
at_last f g x =
    case x of
      [] -> []
      [i] -> [g i]
      i:x' -> f i : at_last f g x'

-- * NRT

-- | Merge two NRT scores.  Retains internal 'nrt_end' messages.
nrt_merge :: NRT -> NRT -> NRT
nrt_merge (NRT p) (NRT q) = NRT (L.merge p q)

-- | Merge a set of NRT.  Retains internal 'nrt_end' messages.
nrt_merge_set :: [NRT] -> NRT
nrt_merge_set = foldr nrt_merge nrt_empty

-- | The empty NRT.
nrt_empty :: NRT
nrt_empty = NRT []

-- | Add bundle at first permissable location of NRT.
nrt_insert_pre :: Bundle -> NRT -> NRT
nrt_insert_pre p (NRT q) = NRT (insert p q)

-- | Add bundle at last permissable location of NRT.
nrt_insert_post :: Bundle -> NRT -> NRT
nrt_insert_post p (NRT q) = NRT (insert_post p q)

-- | 'bundleTime' of 'last' of 'nrt_bundles'.
nrt_end_time :: NRT -> Time
nrt_end_time = bundleTime . last . nrt_bundles

-- | Apply temporal and message functions to bundle.
bundle_map :: (Time -> Time) -> ([Message] -> [Message]) -> Bundle -> Bundle
bundle_map t_f m_f (Bundle t m) = Bundle (t_f t) (m_f m)

-- | Delete any internal 'nrt_end' messages, and require one at the
-- final bundle.
nrt_close :: NRT -> NRT
nrt_close (NRT l) =
    let is_nrt_end_msg = (== "/nrt_end") . messageAddress
        rem_end_msg = bundle_map id (filter (not . is_nrt_end_msg))
        req_end_msg = let f m = if any is_nrt_end_msg m
                                then m
                                else m ++ [nrt_end]
                      in bundle_map id f
    in NRT (at_last rem_end_msg req_end_msg l)
