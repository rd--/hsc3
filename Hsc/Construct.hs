module Hsc.Construct where

import Hsc.UGen
import Hsc.MCE

r0 = UId 0
r1 = UId 1

consU r n i o s id = proxy (mced u)
    where u = UGen r n i o s id

mkOsc r c i o s id = consU r c i o' s id
    where o' = replicate o r

mkFilter c i o s id = consU r c i o' s id
    where r = maximum (map rateOf i)
          o'= replicate o r

mcel (MCE l) = l
mcel u       = [u]

mkOscMCE r c i j o s id = mkOsc r c (i ++ mcel j) o s id

mkFilterMCE c i j o s id = mkFilter c (i ++ mcel j) o s id
