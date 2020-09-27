import Sound.SC3 {- hsc3 -}
import qualified Sound.SC3.UGen.Bindings.DB.RDU as RDU {- sc3-rdu -}

-- > Sound.SC3.Plot.plot_fn_r1_ln (\x -> hs_moddif x 0 1) (0,4)
hs_moddif :: RealFrac a => a -> a -> a -> a
hs_moddif i j m =
  let d = absdif i j `sc3_mod` m
      h = m * 0.5
  in h - absdif d h

-- > Sound.SC3.Plot.plot_fn_r1_ln (\x -> sc3_modDif x 0 1) (0,4)
sc3_modDif :: BinaryOp a => a -> a -> a -> a
sc3_modDif i j m =
  let d = absDif i j `modE` m
      h = m * 0.5
  in h - absDif d h

f_01 f = fSinOsc AR 440 0 * (f 0.2 (fSinOsc AR 2 0 * 0.5) 1)
g_01 = f_01 (modDif KR)
g_02 = f_01 sc3_modDif

-- different moduli
g_03 =
  let sig = lfSaw AR 10 0
      dist = modDif KR sig 0 (mce [0..8] * mouseX KR 0 (1/5) Linear 0.2)
  in splay (sinOsc AR (dist * 4000 + 400) 0) 1 1 0 True * 0.1

-- wrapping amplitude crossfade
g_04 =
  let nc = 12
      nc_u = constant nc
      x = sinOsc AR (RDU.randN nc 'α' 300 800) 0
      d = modDif KR (mouseX KR 0 (nc_u * 2) Linear 0.2) (mce [0 .. nc_u - 1]) nc_u
  in splay (x * max 0 (1 - d)) 1 1 0 True * 0.1
