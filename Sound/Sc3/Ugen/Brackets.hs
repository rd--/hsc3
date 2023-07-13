-- | Brackets
module Sound.Sc3.Ugen.Brackets where

import Data.Bifunctor {- base -}

import qualified Sound.Osc.Packet as Osc {- hosc -}

{- | Brackets are two sets of Open Sound Control messages that can be associated with a Ugen.
The first is to be run prior to the graph being executed, the other after it has ended.
-}
type Brackets = ([Osc.Message], [Osc.Message])

-- | No messages.
emptyBrackets :: Brackets
emptyBrackets = ([], [])

{- | Combine a sequence of Brackets into one Bracket.

>>> f = Data.Bifunctor.bimap concat concat . unzip
>>> f [(['a'],['A']),(['b'],['B'])]
("ab","AB")
-}
concatBrackets :: [Brackets] -> Brackets
concatBrackets = bimap concat concat . unzip
