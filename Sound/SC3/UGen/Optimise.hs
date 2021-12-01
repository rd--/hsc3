-- | Optimisations of UGen graphs.
module Sound.SC3.UGen.Optimise where

import System.Random {- random -}

import Sound.SC3.Common.Math.Operator
import Sound.SC3.Common.Rate
import qualified Sound.SC3.UGen.Bindings.DB as Bindings {- hsc3 -}
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | Constant form of 'rand' UGen.
c_rand :: Random a => Int -> a -> a -> a
c_rand z l r = fst (randomR (l,r) (mkStdGen z))

-- | Constant form of 'iRand' UGen.
c_irand :: (Num b, RealFrac a, Random a) => Int -> a -> a -> b
c_irand z l r = fromInteger (round (c_rand z l r))

{- | Optimise 'UGen' graph by re-writing 'rand' and 'iRand' UGens that
 have 'Constant' inputs.  This, of course, changes the nature of the
 graph, it is no longer randomised at the server.  It's a useful
 transformation for very large graphs which are being constructed
 and sent each time the graph is played.

> import Sound.SC3.UGen.Dot {- hsc3-dot -}

> let u = sinOsc ar (randId 'a' 220 440) 0 * 0.1
> draw (u + ugen_optimise_ir_rand u)
-}
ugen_optimise_ir_rand :: UGen -> UGen
ugen_optimise_ir_rand =
  let f u =
        case u of
          UGen (CPrimitive p) ->
            case p of
              Primitive
                InitialisationRate
                "Rand"
                [UGen (CConstant (Constant l ([],[])))
                ,UGen (CConstant (Constant r ([],[])))]
                [InitialisationRate]
                _
                (UId z)
                ([],[]) -> UGen (CConstant (Constant (c_rand z l r) ([],[])))
              Primitive
                InitialisationRate
                "IRand"
                [UGen (CConstant (Constant l ([],[])))
                ,UGen (CConstant (Constant r ([],[])))]
                [InitialisationRate]
                _
                (UId z)
                ([],[]) -> UGen (CConstant (Constant (c_irand z l r) ([],[])))
              _ -> u
          _ -> u
        in ugenTraverse (const False) f

{- | Optimise 'UGen' graph by re-writing binary operators with
 'Constant' inputs.  The standard graph constructors already do
 this, however subsequent optimisations, ie. 'ugen_optimise_ir_rand'
 can re-introduce these sub-graphs, and the /Plain/ graph
 constructors are un-optimised.

> let u = constant
> u 5 * u 10 == u 50
> u 5 ==** u 5 == u 1
> u 5 >** u 4 == u 1
> u 5 <=** u 5 == u 1
> abs (u (-1)) == u 1
> u 5 / u 2 == u 2.5
> min (u 2) (u 3) == u 2
> max (u 1) (u 3) == u 3

> let u = lfPulse ar (2 ** randId 'α' (-9) 1) 0 0.5
> let u' = ugen_optimise_ir_rand u
> draw (mix (mce [u,u',ugen_optimise_const_operator u']))

> ugen_optimise_const_operator (Bindings.mulAdd 3 1 0) == 3
-}
ugen_optimise_const_operator :: UGen -> UGen
ugen_optimise_const_operator =
  let f u =
        case u of
          UGen (CPrimitive p) ->
            case p of
              Primitive
                _
                "BinaryOpUGen"
                [UGen (CConstant (Constant l ([],[])))
                ,UGen (CConstant (Constant r ([],[])))]
                [_]
                (Special z)
                _
                ([],[]) -> case binop_special_hs z of
                             Just fn -> UGen (CConstant (Constant (fn l r) ([],[])))
                             _ -> u
              Primitive
                _
                "UnaryOpUGen"
                [UGen (CConstant (Constant i ([],[])))]
                [_]
                (Special z)
                _
                ([],[]) -> case uop_special_hs z of
                             Just fn -> UGen (CConstant (Constant (fn i) ([],[])))
                             _ -> u
              Primitive _ "MulAdd" [i, m, a] [_] _ _ ([],[]) -> mulAddOptimised i m a
              _ -> u
          _ -> u
  in ugenTraverse (const False) f

-- | 'u_constant' of 'ugen_optimise_ir_rand'.
constant_opt :: UGen -> Maybe Sample
constant_opt = u_constant . ugen_optimise_ir_rand

{- | Constant optimising MulAdd.

> mulAddOptimised (sinOsc ar 440 0) 1 0 == sinOsc ar 440 0
> mulAddOptimised (sinOsc ar 440 0) 0.1 0 == sinOsc ar 440 0 * 0.1
> mulAddOptimised (sinOsc ar 440 0) 1 1 == sinOsc ar 440 0 + 1
> mulAddOptimised (sinOsc ar 440 0) 0.1 1 == mulAdd (sinOsc ar 440 0) 0.1 1
-}
mulAddOptimised :: UGen -> UGen -> UGen -> UGen
mulAddOptimised u m a =
  case (is_constant_of 1 m,is_constant_of 0 a) of
    (True,True) -> u
    (False,True) -> u * m
    (True,False) -> u + a
    (False,False) -> Bindings.mulAdd u m a
