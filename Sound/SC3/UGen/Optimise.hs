-- | Optimisations of UGen graphs.
module Sound.SC3.UGen.Optimise where

import System.Random {- random -}

import Sound.SC3.UGen.Math
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | Constant form of 'rand' UGen.
c_rand :: Random a => Int -> a -> a -> a
c_rand z l r = fst (randomR (l,r) (mkStdGen z))

-- | Constant form of 'iRand' UGen.
c_irand :: (Num b, RealFrac a, Random a) => Int -> a -> a -> b
c_irand z l r = fromInteger (round (c_rand z l r))

-- | Optimise 'UGen' graph by re-writing 'rand' and 'iRand' UGens that
-- have 'Constant' inputs.  This, of course, changes the nature of the
-- graph, it is no longer randomised at the server.  It's a useful
-- transformation for very large graphs which are being constructed
-- and sent each time the graph is played.
--
-- > import Sound.SC3.UGen.Dot
--
-- > let u = sinOsc AR (rand 'a' 220 440) 0 * 0.1
-- > in draw (u + ugen_optimise_ir_rand u)
ugen_optimise_ir_rand :: UGen -> UGen
ugen_optimise_ir_rand =
    let f u =
            case u of
              Primitive_U p ->
                  case p of
                    Primitive IR "Rand" [Constant_U (Constant l),Constant_U (Constant r)] [IR] _ (UId z) ->
                        Constant_U (Constant (c_rand z l r))
                    Primitive IR "IRand" [Constant_U (Constant l),Constant_U (Constant r)] [IR] _ (UId z) ->
                        Constant_U (Constant (c_irand z l r))
                    _ -> u
              _ -> u
    in ugenTraverse f

-- | Optimise 'UGen' graph by re-writing binary operators with
-- 'Constant' inputs.  The standard graph constructors already do
-- this, however subsequent optimisations, ie. 'ugen_optimise_ir_rand'
-- can re-introduce these sub-graphs, and the /Plain/ graph
-- constructors are un-optimised.
--
-- > constant 5 * 10 == constant 50
--
-- > let {u = lfPulse AR (2 ** rand 'α' (-9) 1) 0 0.5
-- >     ;u' = ugen_optimise_ir_rand u}
-- > in draw (mix (mce [u,u',ugen_optimise_const_operator u']))
ugen_optimise_const_operator :: UGen -> UGen
ugen_optimise_const_operator =
    let f u =
            case u of
              Primitive_U p ->
                  case p of
                    Primitive _ "BinaryOpUGen" [Constant_U (Constant l),Constant_U (Constant r)] [_] (Special z) _ ->
                        case binop_special_hs z of
                          Just fn -> Constant_U (Constant (fn l r))
                          _ -> u
                    Primitive _ "UnaryOpUGen" [Constant_U (Constant i)] [_] (Special z) _ ->
                        case uop_special_hs z of
                          Just fn -> Constant_U (Constant (fn i))
                          _ -> u
                    _ -> u
              _ -> u
    in ugenTraverse f
