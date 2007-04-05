module Sound.SC3.UGen.UId where

import Control.Monad
import Data.Unique

class (Monad m) => UId m where
   uid :: m Int

instance UId IO where
   uid = liftM hashUnique newUnique
