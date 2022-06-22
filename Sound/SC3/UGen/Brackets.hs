-- | Brackets
module Sound.SC3.UGen.Brackets where

import Data.Bifunctor {- base -}

import qualified Sound.Osc as Osc {- hosc -}

{- | Brackets are two sets of Open Sound Control messages that can be associated with a UGen.
The first is to be run prior to the graph being executed, the other after it has ended.
-}
type Brackets = ([Osc.Message], [Osc.Message])

-- | No messages.
emptyBrackets :: Brackets
emptyBrackets = ([], [])

{- | Combine a sequence of Brackets into one Bracket.

> f = bimap concat concat . unzip
> f [(['a'],['A']),(['b'],['B'])]
-}
concatBrackets :: [Brackets] -> Brackets
concatBrackets = bimap concat concat . unzip
