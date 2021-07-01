-- sc-users, 2007-04-06 (pj) [paul jones]
let insects _ =
      let o = sinOsc kr (lfNoise2 kr 50 * 50 + 50) 0 * 100 + 2000
      in bpf (brownNoise ar) o 0.001 * 10
in mceFill 2 insects

-- sc-users, 2007-04-06 (pj) [paul jones] ; monad
uid_st_eval (do
  let insects_m :: UId m => m UGen
      insects_m = do
        n1 <- brownNoiseM ar
        n2 <- lfNoise2M kr 50
        let o = sinOsc kr (n2 * 50 + 50) 0 * 100 + 2000
        return (bpf n1 o 0.001 * 10)
  fmap mce (replicateM 2 insects_m))
