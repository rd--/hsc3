-- | Nrt editing
module Sound.SC3.Server.Nrt.Edit where

import Data.List {- base -}
import qualified Data.List.Ordered as L {- data-ordlist -}
import Sound.OSC {- hosc -}

import Sound.SC3.Server.Command
import Sound.SC3.Server.Nrt

-- * List

{- | Inserts at the first position where it compares less but not equal to the next element.

> import Data.Function {- base -}
> insertBy (compare `on` fst) (3,'x') (zip [1..5] ['a'..])
> insertBy_post (compare `on` fst) (3,'x') (zip [1..5] ['a'..])
-}
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

-- * Nrt

-- | Merge two Nrt scores.  Retains internal 'nrt_end' messages.
nrt_merge :: Nrt -> Nrt -> Nrt
nrt_merge (Nrt p) (Nrt q) = Nrt (L.merge p q)

-- | Merge a set of Nrt.  Retains internal 'nrt_end' messages.
nrt_merge_set :: [Nrt] -> Nrt
nrt_merge_set = foldr nrt_merge nrt_empty

-- | The empty Nrt.
nrt_empty :: Nrt
nrt_empty = Nrt []

-- | Add bundle at first permissable location of Nrt.
nrt_insert_pre :: Bundle -> Nrt -> Nrt
nrt_insert_pre p (Nrt q) = Nrt (insert p q)

-- | Add bundle at last permissable location of Nrt.
nrt_insert_post :: Bundle -> Nrt -> Nrt
nrt_insert_post p (Nrt q) = Nrt (insert_post p q)

-- | 'bundleTime' of 'last' of 'nrt_bundles'.
nrt_end_time :: Nrt -> Time
nrt_end_time = bundleTime . last . nrt_bundles

-- | Apply temporal and message functions to bundle.
bundle_map :: (Time -> Time) -> ([Message] -> [Message]) -> Bundle -> Bundle
bundle_map t_f m_f (Bundle t m) = Bundle (t_f t) (m_f m)

-- | Delete any internal 'nrt_end' messages, and require one at the final bundle.
nrt_close :: Nrt -> Nrt
nrt_close (Nrt l) =
    let is_nrt_end_msg = (== "/nrt_end") . messageAddress
        rem_end_msg = bundle_map id (filter (not . is_nrt_end_msg))
        req_end_msg = let f m = if any is_nrt_end_msg m
                                then m
                                else m ++ [nrt_end]
                      in bundle_map id f
    in Nrt (at_last rem_end_msg req_end_msg l)

-- | Append /q/ to /p/, assumes last timestamp at /p/ precedes first at /q/.
nrt_append :: Nrt -> Nrt -> Nrt
nrt_append (Nrt p) (Nrt q) = Nrt (p ++ q)
