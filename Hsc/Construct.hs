module Hsc.Construct where

import Hsc.UId (UId(UId))
import Hsc.UGen (UGen(UGen, MCE), rateOf, proxy)
import Hsc.MCE (mced)
import Data.Unique (newUnique, hashUnique)

mkUId :: IO Int
mkUId = do u <- newUnique
           return (hashUnique u)

uniquify :: UGen -> IO UGen
uniquify (UGen r n i o s _) = do uid <- mkUId
                                 return (UGen r n i o s (UId uid))
uniquify (MCE u)            = do u' <- mapM uniquify u
                                 return (MCE u')
uniquify u                  = error ("uniquify: illegal value" ++ show u)

consU r n i o s id = proxy (mced u)
    where u = UGen r n i o s id

mkOsc r c i o s = consU r c i o' s (UId 0)
    where o' = replicate o r

mkOsc' r c i o s id = consU r c i o' s id
    where o' = replicate o r

mkFilter c i o s = consU r c i o' s (UId 0)
    where r = maximum (map rateOf i)
          o'= replicate o r

mcel (MCE l) = l
mcel u       = [u]

mkOscMCE r c i j o s = mkOsc r c (i ++ mcel j) o s

mkFilterMCE c i j o s = mkFilter c (i ++ mcel j) o s

--

dupn n u  = do d  <- mapM uniquify (take n (repeat u))
               return (MCE d)

dupn' n u = do u' <- u
               d  <- mapM uniquify (take n (repeat u'))
               return (MCE d)

dup  = dupn 2

dup' = dupn' 2
