module Sound.SC3.UGen.UId where

import Control.Monad
import Data.Unique

data UId = UId Int deriving (Eq, Show)

class UIdGen m where
   uid :: m UId

instance UIdGen IO where
   uid = liftM (UId . hashUnique) newUnique

-- | The nil identifier.
zeroUId :: UId
zeroUId = UId 0

n_uid :: (Monad m, UIdGen m) => Int -> m [UId]
n_uid n = replicateM n uid

uid2 :: (Monad m, UIdGen m) => m (UId, UId)
uid2 = return . (\[a,b] -> (a,b)) =<< replicateM 2 uid

uid3 :: (Monad m, UIdGen m) => m (UId, UId, UId)
uid3 = return . (\[a,b,c] -> (a,b,c)) =<< replicateM 3 uid

uid4 :: (Monad m, UIdGen m) => m (UId, UId, UId, UId)
uid4 = return . (\[a,b,c,d] -> (a,b,c,d)) =<< replicateM 4 uid

uid5 :: (Monad m, UIdGen m) => m (UId, UId, UId, UId, UId)
uid5 = return . (\[a,b,c,d,e] -> (a,b,c,d,e)) =<< replicateM 5 uid

uid6 :: (Monad m, UIdGen m) => m (UId, UId, UId, UId, UId, UId)
uid6 = return . (\[a,b,c,d,e,f] -> (a,b,c,d,e,f)) =<< replicateM 6 uid

uid7 :: (Monad m, UIdGen m) => m (UId, UId, UId, UId, UId, UId, UId)
uid7 = return . (\[a,b,c,d,e,f,g] -> (a,b,c,d,e,f,g)) =<< replicateM 7 uid

uid8 :: (Monad m, UIdGen m) => m (UId, UId, UId, UId, UId, UId, UId, UId)
uid8 = return . (\[a,b,c,d,e,f,g,h] -> (a,b,c,d,e,f,g,h)) =<< replicateM 8 uid

uid9 :: (Monad m, UIdGen m) => m (UId, UId, UId, UId, UId, UId, UId, UId, UId)
uid9 = return . (\[a,b,c,d,e,f,g,h,i] -> (a,b,c,d,e,f,g,h,i)) =<< replicateM 9 uid
