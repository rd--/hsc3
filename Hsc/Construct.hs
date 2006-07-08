module Hsc.Construct where

import Hsc.UGen (UId(..), UGen(..), rateOf, proxy)
import Hsc.MCE (mced)

import Data.Unique (newUnique, hashUnique)
import Control.Monad (liftM, join)

zeroUId = (UId 0)

mkId :: IO Int
mkId = do u <- newUnique
          return (hashUnique u)

mkUId = do id <- mkId
           return (UId id)

uniquify :: UGen -> IO UGen
uniquify (UGen r n i o s _) = do id <- mkUId
                                 return (UGen r n i o s id)
uniquify (MCE u)            = do u' <- mapM uniquify u
                                 return (MCE u')
uniquify u                  = error ("uniquify: illegal value" ++ show u)

consU r n i o s id = proxy (mced u)
    where u = UGen r n i o s id

mkOsc r c i o s = consU r c i o' s zeroUId
    where o' = replicate o r

mkOsc' r c i o s id = consU r c i o' s id
    where o' = replicate o r

mkFilter c i o s = consU r c i o' s zeroUId
    where r = maximum (map rateOf i)
          o'= replicate o r

mcel (MCE l) = l
mcel u       = [u]

mkOscMCE r c i j o s = mkOsc r c (i ++ mcel j) o s

mkFilterMCE c i j o s = mkFilter c (i ++ mcel j) o s

--

dupn n u  = do d  <- mapM uniquify (take n (repeat u))
               return (MCE d)

dup       = dupn 2
dupn' n u = join ((liftM (dupn n) u))
dup'      = dupn' 2

