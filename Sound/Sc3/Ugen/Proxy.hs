-- | Proxy indicating an output port at a multi-channel Primitive.
module Sound.Sc3.Ugen.Proxy where

import Sound.Sc3.Common.Rate {- hsc3 -}

import Sound.Sc3.Ugen.Primitive {- hsc3 -}

data Proxy t =
  Proxy
  {proxySource :: Primitive t
  ,proxyIndex :: Int}
  deriving (Ord, Eq, Read, Show)

proxyRate :: Proxy t -> Rate
proxyRate = ugenRate . proxySource
