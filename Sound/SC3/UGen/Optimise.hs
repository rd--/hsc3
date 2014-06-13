-- | Optimisations of UGen graphs.
module Sound.SC3.UGen.Optimise where

import Sound.SC3.UGen.Operator
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

import System.Random {- random -}

c_rand :: Random a => Int -> a -> a -> a
c_rand z l r = fst (randomR (l,r) (mkStdGen z))

c_irand :: (Num b, RealFrac a, Random a) => Int -> a -> a -> b
c_irand z l r = fromInteger (round (c_rand z l r))

ugen_optimize_ir_rand :: UGen -> UGen
ugen_optimize_ir_rand =
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

ugen_optimize_const_binop :: UGen -> UGen
ugen_optimize_const_binop =
    let f u =
            case u of
              Primitive_U p ->
                  case p of
                    Primitive _ "BinaryOpUGen" [Constant_U (Constant l),Constant_U (Constant r)] [_] (Special z) _ ->
                        case binop_special_hs z of
                          Just fn -> Constant_U (Constant (fn l r))
                          _ -> u
                    _ -> u
              _ -> u
    in ugenTraverse f
