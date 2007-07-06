module Sound.SC3.UGen.UGen.Construct ( mkUnaryOperator, mkBinaryOperator
                                     , mkOscId, mkOsc
                                     , mkOscMCEId, mkOscMCE
                                     , mkFilterId, mkFilter, mkFilterKeyed
                                     , mkFilterMCE
                                     , liftU, liftU2, liftU3, liftU4 ) where

import Sound.SC3.UGen.Operator
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.MCE
import Sound.SC3.UGen.UId

-- * UGen Constructors.

-- | Apply proxy transformation if required.
proxy :: UGen -> UGen
proxy (MCE l) = MCE (map proxy l)
proxy u@(UGen _ _ _ o _ _) =
   case o of
      (_:_:_) -> MCE (map (Proxy u) [0..(length o - 1)])
      _       -> u
proxy _       = error "proxy: illegal ugen"

-- | Determine the rate of a UGen.
rateOf :: UGen -> Rate
rateOf (Constant _)        =  IR
rateOf (Control r _ _)     =  r
rateOf (UGen r _ _ _ _ _)  =  r
rateOf (Proxy u _)         =  rateOf u
rateOf (MCE u)             =  maximum (map rateOf u)
rateOf (MRG _)             =  error "rateOf: applied to MRG"

-- | Construct proxied and multiple channel expanded UGen.
mkUGen :: Rate -> Name -> [UGen] -> [Output] -> Special -> UGenId -> UGen
mkUGen r n i o s z = proxy (mceExpand u)
    where u = UGen r n i o s z

-- | Operator UGen constructor.
mkOperator :: Name -> [UGen] -> Int -> UGen
mkOperator c i s = mkUGen r c i [r] (Special s) (UGenId 0)
    where r = maximum (map rateOf i)

-- | Unary math constructor with constant optimization.
mkUnaryOperator :: Unary -> (Double -> Double) -> UGen -> UGen
mkUnaryOperator _ f (Constant a) = Constant (f a)
mkUnaryOperator i _ a = mkOperator "UnaryOpUGen" [a] (fromEnum i)

-- | Binary math constructor with constant optimization.
mkBinaryOperator :: Binary -> (Double -> Double -> Double) -> UGen -> UGen -> UGen
mkBinaryOperator _ f (Constant a) (Constant b) = Constant (f a b)
mkBinaryOperator i _ a b = mkOperator "BinaryOpUGen" [a, b] (fromEnum i)

-- | Oscillator constructor.
mkOscId :: UGenId -> Rate -> Name -> [UGen] -> Int -> UGen
mkOscId z r c i o = mkUGen r c i (replicate o r) (Special 0) z

-- | Oscillator constructor.
mkOsc :: Rate -> Name -> [UGen] -> Int -> UGen
mkOsc = mkOscId (UGenId 0)

-- | Variant oscillator constructor with MCE collapsing input.
mkOscMCEId :: UGenId -> Rate -> Name -> [UGen] -> UGen -> Int -> UGen
mkOscMCEId z r c i j o = mkOscId z r c (i ++ mceChannels j) o

-- | Variant oscillator constructor with MCE collapsing input.
mkOscMCE :: Rate -> Name -> [UGen] -> UGen -> Int -> UGen
mkOscMCE = mkOscMCEId (UGenId 0)

-- | Filter UGen constructor.
mkFilterId :: UGenId -> Name -> [UGen] -> Int -> UGen
mkFilterId z c i o = mkUGen r c i o' (Special 0) z
    where r = maximum (map rateOf i)
          o'= replicate o r

-- | Filter UGen constructor.
mkFilter :: Name -> [UGen] -> Int -> UGen
mkFilter = mkFilterId (UGenId 0)

-- | Variant filter with rate derived from keyed input.
mkFilterKeyed :: Name -> Int -> [UGen] -> Int -> UGen
mkFilterKeyed c k i o = mkUGen r c i o' (Special 0) (UGenId 0)
    where r = rateOf (i !! k)
          o' = replicate o r

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCE :: Name -> [UGen] -> UGen -> Int -> UGen
mkFilterMCE c i j o = mkFilter c (i ++ mceChannels j) o

-- | Lifting UGenId requiring UGens to UId
liftU :: (UId m) => (UGenId -> a -> UGen) -> (a -> m UGen)
liftU f a = do n <- generateUId
               return (f (UGenId n) a)

liftU2 :: (UId m) => (UGenId -> a -> b -> UGen) -> (a -> b -> m UGen)
liftU2 f a b = do n <- generateUId
                  return (f (UGenId n) a b)

liftU3 :: (UId m) => (UGenId -> a -> b -> c -> UGen) -> (a -> b -> c -> m UGen)
liftU3 f a b c = do n <- generateUId
                    return (f (UGenId n) a b c)

liftU4 :: (UId m) => (UGenId -> a -> b -> c -> d -> UGen) -> (a -> b -> c -> d -> m UGen)
liftU4 f a b c d = do n <- generateUId
                      return (f (UGenId n) a b c d)

