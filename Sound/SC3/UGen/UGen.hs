module Sound.SC3.UGen.UGen ( Name, UGenId(..), UGen(..), Output, Special(..)
                           , constant, control, mce, mrg, proxy
                           , clone ) where

import Control.Monad (liftM, replicateM)
import Sound.SC3.UGen.Rate (Rate)
import Sound.SC3.UGen.UId

type Name = String
type Output = Rate
newtype Special = Special Int deriving (Eq, Show)
newtype UGenId = UGenId Int deriving (Eq, Show)
data UGen = Constant { constantValue :: Double }
          | Control { controlRate_ :: Rate
                    , controlName :: Name
                    , controlDefault :: Double }
          | Primitive { ugenRate :: Rate
                      , ugenName :: Name
                      , ugenInputs :: [UGen]
                      , ugenOutputs :: [Output]
                      , ugenSpecial :: Special
                      , ugenId :: UGenId }
          | Proxy { proxySource :: UGen
                  , proxyIndex :: Int }
          | MCE { mceProxies :: [UGen] }
          | MRG { mrgLeft :: UGen 
                , mrgRight :: UGen }
            deriving (Eq, Show)

-- | Constant value constructor.
constant :: (Real a) => a -> UGen
constant = Constant . realToFrac

-- | Control input constructor.
control :: Rate -> Name -> Double -> UGen
control = Control

-- | Multiple channel expansion constructor.
mce :: [UGen] -> UGen
mce = MCE

-- | Variant multiple root graph constructor.
mrg :: [UGen] -> UGen
mrg [] = undefined
mrg [x] = x
mrg (x:xs) = MRG x (mrg xs)

proxy :: UGen -> Int -> UGen
proxy = Proxy

-- | Clone UGen.
clone :: (UId m) => Int -> m UGen -> m UGen
clone n u = liftM MCE (replicateM n u)
