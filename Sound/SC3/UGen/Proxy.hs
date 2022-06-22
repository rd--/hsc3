-- | Proxy indicating an output port at a multi-channel Primitive.
module Sound.SC3.UGen.Proxy where

import Sound.SC3.Common.Rate {- hsc3 -}

import Sound.SC3.UGen.Primitive {- hsc3 -}

data Proxy t =
  Proxy
  {proxySource :: Primitive t
  ,proxyIndex :: Int}
  deriving (Ord, Eq, Read, Show)

proxyRate :: Proxy t -> Rate
proxyRate = ugenRate . proxySource
