module Sound.SC3.UGen.UGen.Construct ( mkUnaryOperator, mkBinaryOperator
                                     , mkOscId, mkOsc
                                     , mkOscMCEId, mkOscMCE
                                     , mkFilterId, mkFilter, mkFilterKeyed
                                     , mkFilterMCEId, mkFilterMCE
                                     , mkInfo ) where

import Sound.SC3.UGen.Operator
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.MCE
import Sound.SC3.UGen.UGen.Predicate

-- * UGen Constructors.

-- | Apply proxy transformation if required.
proxify :: UGen -> UGen
proxify u 
    | isMCE u = mce (map proxify (mceProxies u))
    | isMRG u = mrg [proxify (mrgLeft u), mrgRight u]
    | isUGen u = let o = ugenOutputs u
                 in case o of
                      (_:_:_) -> mce (map (proxy u) [0..(length o - 1)])
                      _ -> u
    | otherwise = error "proxify: illegal ugen"

-- | Determine the rate of a UGen.
rateOf :: UGen -> Rate
rateOf u
    | isConstant u = IR
    | isControl u = controlOperatingRate u
    | isUGen u = ugenRate u
    | isProxy u = rateOf (proxySource u)
    | isMCE u = maximum (map rateOf (mceProxies u))
    | isMRG u = rateOf (mrgLeft u)
    | otherwise = undefined

-- | True is input is a sink UGen, ie. has no outputs.
isSink :: UGen -> Bool
isSink u
    | isUGen u = null (ugenOutputs u)
    | isMCE u = all isSink (mceProxies u)
    | isMRG u = isSink (mrgLeft u)
    | otherwise = False

-- | Ensure input UGen is valid, ie. not a sink.
checkInput :: UGen -> UGen
checkInput u = if isSink u then error ("illegal input" ++ show u) else u

-- | Construct proxied and multiple channel expanded UGen.
mkUGen :: Rate -> String -> [UGen] -> [Output] -> Special -> Maybe UGenId -> UGen
mkUGen r n i o s z = proxify (mceExpand u)
    where u = Primitive r n (map checkInput i) o s z

-- | Operator UGen constructor.
mkOperator :: String -> [UGen] -> Int -> UGen
mkOperator c i s = mkUGen r c i [r] (Special s) Nothing
    where r = maximum (map rateOf i)

-- | Unary math constructor with constant optimization.
mkUnaryOperator :: Unary -> (Double -> Double) -> UGen -> UGen
mkUnaryOperator i f a 
    | isConstant a = constant (f (constantValue a))
    | otherwise = mkOperator "UnaryOpUGen" [a] (fromEnum i)

-- | Binary math constructor with constant optimization.
mkBinaryOperator :: Binary -> (Double -> Double -> Double) -> UGen -> UGen -> UGen
mkBinaryOperator i f a b 
    | isConstant a && isConstant b = let a' = constantValue a
                                         b' = constantValue b
                                     in constant (f a' b')
    | otherwise = mkOperator "BinaryOpUGen" [a, b] (fromEnum i)

mkOsc_ :: Maybe UGenId -> Rate -> String -> [UGen] -> Int -> UGen
mkOsc_ z r c i o = mkUGen r c i (replicate o r) (Special 0) z

-- | Oscillator constructor.
mkOsc :: Rate -> String -> [UGen] -> Int -> UGen
mkOsc = mkOsc_ Nothing

-- | Oscillator constructor, setting identifier.
mkOscId :: UGenId -> Rate -> String -> [UGen] -> Int -> UGen
mkOscId z = mkOsc_ (Just z)

mkOscMCE_ :: Maybe UGenId -> Rate -> String -> [UGen] -> UGen -> Int -> UGen
mkOscMCE_ z r c i j o = mkOsc_ z r c (i ++ mceChannels j) o

-- | Variant oscillator constructor with MCE collapsing input.
mkOscMCE :: Rate -> String -> [UGen] -> UGen -> Int -> UGen
mkOscMCE = mkOscMCE_ Nothing

-- | Variant oscillator constructor with MCE collapsing input.
mkOscMCEId :: UGenId -> Rate -> String -> [UGen] -> UGen -> Int -> UGen
mkOscMCEId z = mkOscMCE_ (Just z)

mkFilter_ :: Maybe UGenId -> String -> [UGen] -> Int -> UGen
mkFilter_ z c i o = mkUGen r c i o' (Special 0) z
    where r = maximum (map rateOf i)
          o'= replicate o r

-- | Filter UGen constructor.
mkFilter :: String -> [UGen] -> Int -> UGen
mkFilter = mkFilter_ Nothing

-- | Filter UGen constructor.
mkFilterId :: UGenId -> String -> [UGen] -> Int -> UGen
mkFilterId z = mkFilter_ (Just z)

-- | Variant filter with rate derived from keyed input.
mkFilterKeyed :: String -> Int -> [UGen] -> Int -> UGen
mkFilterKeyed c k i o = mkUGen r c i o' (Special 0) Nothing
    where r = rateOf (i !! k)
          o' = replicate o r

mkFilterMCE_ :: Maybe UGenId -> String -> [UGen] -> UGen -> Int -> UGen
mkFilterMCE_ z c i j o = mkFilter_ z c (i ++ mceChannels j) o

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCE :: String -> [UGen] -> UGen -> Int -> UGen
mkFilterMCE = mkFilterMCE_ Nothing

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCEId :: UGenId -> String -> [UGen] -> UGen -> Int -> UGen
mkFilterMCEId z = mkFilterMCE_ (Just z)

-- | Information unit generators are very specialized.
mkInfo :: String -> UGen
mkInfo name = mkOsc IR name [] 1
