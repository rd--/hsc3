module Hsc.UGen where

data Rate     = IR | KR | AR
                deriving (Eq, Ord, Show)

type Output   = Rate

type Special  = Int

data UGen     = Constant Float
              | Control Rate String Float
              | UGen Rate String [UGen] [Output] Special Int
              | Proxy UGen Int
              | MCE [UGen]
                deriving (Eq, Show)

data UType    = ConstantT | ControlT | UGenT | ProxyT | MCET
                deriving (Eq, Show)

rate :: UGen -> Rate
rate (Constant _)       =  IR
rate (Control r _ _)    =  r
rate (UGen r _ _ _ _ _) =  r
rate (Proxy u _)        =  rate u
rate (MCE u)            =  maximum $ map rate u

rateId :: Rate -> Int
rateId IR = 0
rateId KR = 1
rateId AR = 2

nodes :: UGen -> [UGen]
nodes u@(UGen _ _ i _ _ _)  =  u : concatMap nodes i
nodes (Proxy u _)           =  u : nodes u
nodes (MCE u)               =  concatMap nodes u
nodes u                     =  [u]

-- Apply depth first.

traverseu :: (UGen -> UGen) -> UGen -> UGen
traverseu f (UGen r n i o s id) = f (UGen r n (map (traverseu f) i) o s id)
traverseu f (MCE l)             = f (MCE (map (traverseu f) l))
traverseu f (Proxy u n)         = f (Proxy (traverseu f u) n)
traverseu f u                   = f u

utype :: UGen -> UType
utype (Constant _)          = ConstantT
utype (Control _ _ _)       = ControlT
utype (UGen _ _ _ _ _ _)    = UGenT
utype (Proxy _ _)           = ProxyT
utype (MCE _)               = MCET

isConstant, isControl, isUGen :: UGen -> Bool
isConstant u                  = utype u == ConstantT
isControl u                   = utype u == ControlT
isUGen u                      = utype u == UGenT
isProxy u                     = utype u == ProxyT
isMCE u                       = utype u == MCET

proxy :: UGen -> UGen
proxy u@(UGen _ _ _ o _ _) = (MCE (map f [0..(n-1)]))
    where f i = (Proxy u i)
          n   = length o

proxyU :: Rate -> String -> [UGen] -> [Output] -> Special -> Int -> UGen
proxyU r n i o s id = proxy (UGen r n i o s id)
