module Sound.SC3.UGen.UGen ( Name, UGenId(..), UGen(..), Output, Special(..)
                           , clone ) where

import Control.Monad (liftM, replicateM)
import Sound.SC3.UGen.Rate (Rate)
import Sound.SC3.UGen.UId

type Name    = String
type Output  = Rate
newtype Special = Special Int deriving (Eq, Show)
newtype UGenId = UGenId Int deriving (Eq, Show)
data UGen    = Constant { constantValue :: Double }
             | Control { controlRate_ :: Rate
                       , controlName :: Name
                       , controlDefault :: Double }
             | UGen { ugenRate :: Rate
                    , ugenName :: Name
                    , ugenInputs :: [UGen]
                    , ugenOuputs :: [Output]
                    , ugenSpecial :: Special
                    , ugenId :: UGenId }
             | Proxy { proxySource :: UGen
                     , proxyIndex :: Int }
             | MCE { mceProxies :: [UGen] }
             | MRG { mrgRoots :: [UGen] }
               deriving (Eq, Show)

-- | Clone UGen.
clone :: (UId m) => Int -> m UGen -> m UGen
clone n u = liftM MCE (replicateM n u)
