module Hsc.Envelope where

import Hsc.UGen

envgen r gate lvl bias scale done env = 
    UGen r "EnvGen" ([gate,lvl,bias,scale,done] ++ env) [r] 0 0

data EnvCurve = EnvStep 
              | EnvLin | EnvExp 
              | EnvSin | EnvCos 
              | EnvNum
              | EnvSqr | EnvCub
              deriving (Eq, Ord, Show, Enum)

env_curve c = fromIntegral $ fromEnum c

env_value c = 0.0

env (l:vl) (t:m) crv rls lp = 
    [l, t, rls, lp] ++ concat (zipWith3 f vl m (take n $ cycle crv))
    where f    = (\l t c -> [l, t, env_curve c, env_value c])
          n    = length (t:m)
          
envperc atk rls lvl crv = env [0.0, lvl, 0.0] [atk, rls] crv (-1.0) (-1.0)

envperc' = envperc 0.01 1.0 1.0 [EnvExp, EnvExp]
