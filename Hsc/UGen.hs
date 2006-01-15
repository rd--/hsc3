module Hsc.UGen where

data Rate     = IR | KR | AR | DR
                deriving (Eq, Show)
type Output   = Rate
type Special  = Int
data UId      = UId Int
                deriving (Eq, Show)
data UGen     = Constant Float
              | Control Rate String Float
              | UGen Rate String [UGen] [Output] Special UId
              | Proxy UGen Int
              | MCE [UGen]
              | MRG [UGen]
                deriving (Eq, Show)
data UType    = ConstantT | ControlT | UGenT | ProxyT | MCET | MRGT
                deriving (Eq, Show)

rateOrd IR = 0
rateOrd DR = 1
rateOrd KR = 2
rateOrd AR = 3

instance Ord Rate where
    compare a b = compare (rateOrd a) (rateOrd b)

--rate :: UGen -> Rate
rateOf (Constant _)       =  IR
rateOf (Control r _ _)    =  r
rateOf (UGen r _ _ _ _ _) =  r
rateOf (Proxy u _)        =  rateOf u
rateOf (MCE u)            =  maximum $ map rateOf u
rateOf _                  =  error "illegal ugen"

--rateId :: Rate -> Int
rateId IR = 0
rateId KR = 1
rateId AR = 2
rateId DR = 3

--nodes :: UGen -> [UGen]
nodes u@(UGen _ _ i _ _ _)  =  u : concatMap nodes i
nodes (Proxy u _)           =  u : nodes u
nodes (MCE u)               =  concatMap nodes u
nodes (MRG u)               =  concatMap nodes u
nodes u                     =  [u]

-- Apply depth first.

--traverseu :: (UGen -> UGen) -> UGen -> UGen
traverseu f (UGen r n i o s id) = f (UGen r n (map (traverseu f) i) o s id)
traverseu f (MCE l)             = f (MCE (map (traverseu f) l))
traverseu f (MRG l)             = f (MRG (map (traverseu f) l))
traverseu f (Proxy u n)         = f (Proxy (traverseu f u) n)
traverseu f u                   = f u

--utype :: UGen -> UType
utype (Constant _)          = ConstantT
utype (Control _ _ _)       = ControlT
utype (UGen _ _ _ _ _ _)    = UGenT
utype (Proxy _ _)           = ProxyT
utype (MCE _)               = MCET
utype (MRG _)               = MRGT

--isConstant, isControl, isUGen :: UGen -> Bool
isConstant u                  = utype u == ConstantT
isControl u                   = utype u == ControlT
isUGen u                      = utype u == UGenT
isProxy u                     = utype u == ProxyT
isMCE u                       = utype u == MCET
isMRG u                       = utype u == MRGT

--proxy :: UGen -> UGen
proxy (MCE l) = MCE (map proxy l)
proxy u@(UGen _ _ _ o _ _)
    | length o > 1 = (MCE (map f [0..(n-1)]))
    | otherwise    = u
    where f i = (Proxy u i)
          n   = length o
