module Sound.SC3.UGen.UGen.Predicate where

import Sound.SC3.UGen.UGen

-- | Constant predicate.
isConstant :: UGen -> Bool
isConstant (Constant _) = True
isConstant _            = False

-- | Control predicate.
isControl :: UGen -> Bool
isControl (Control _ _ _) = True
isControl _               = False

-- | UGen predicate.
isUGen :: UGen -> Bool
isUGen (UGen _ _ _ _ _ _) = True
isUGen _                  = False

-- | Proxy predicate.
isProxy :: UGen -> Bool
isProxy (Proxy _ _) = True
isProxy _           = False

-- | MCE predicate.
isMCE :: UGen -> Bool
isMCE (MCE _) = True
isMCE _       = False

-- | MRG predicate.
isMRG :: UGen -> Bool
isMRG (MRG _) = True
isMRG _       = False

