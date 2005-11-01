module Hsc.UGen where

data Rate     = IR | KR | AR 
                deriving (Eq, Ord, Show)

type Output   = Rate

type Special  = Int

data UGen     = Constant Float
              | Control Rate String Float 
              | UGen Rate String [UGen] [Output] Special 
              | Proxy UGen Int
              | MCE [UGen] 
                deriving (Eq, Show)

data UType    = ConstantT | ControlT | UGenT | ProxyT | MCET
                deriving (Eq, Show)

rate :: UGen -> Rate
rate (Constant a)     =  IR
rate (Control r _ _)  =  r
rate (UGen r _ _ _ _) =  r
rate (Proxy u i)      =  rate u
rate (MCE u)          =  maximum $ map rate u

rateId :: Rate -> Int
rateId IR = 0
rateId KR = 1
rateId AR = 2

nodes :: UGen -> [UGen]
nodes u@(UGen _ _ i _ _)  =  u : concat (map nodes i)
nodes (Proxy u i)         =  [u]
nodes (MCE u)             =  concat $ map nodes u
nodes u                   =  [u]

-- Apply depth first.

traverseu :: (UGen -> UGen) -> UGen -> UGen
traverseu f (UGen r n i o s) = f (UGen r n (map (traverseu f) i) o s)
traverseu f (MCE l)          = f (MCE (map (traverseu f) l))
traverseu f (Proxy u n)      = f (Proxy (f u) n)
traverseu f u                = f u

utype :: UGen -> UType
utype (Constant _)        = ConstantT
utype (Control _ _ _)     = ControlT
utype (UGen _ _ _ _ _)    = UGenT
utype (Proxy _ _)         = ProxyT
utype (MCE _)             = MCET

isConstant, isControl, isUGen :: UGen -> Bool
isConstant u                  = utype u == ConstantT
isControl u                   = utype u == ControlT
isUGen u                      = utype u == UGenT
isMCE u                       = utype u == MCET
