-- sc-users, 2007-04-06 (pj) [paul jones]
let insects_m :: UId m => m UGen
    insects_m = do
      n1 <- brownNoiseM AR
      n2 <- lfNoise2M KR 50
      let o = sinOsc KR (n2 * 50 + 50) 0 * 100 + 2000
      return (bpf n1 o 0.001 * 10)
    forest_sounds_m :: UId m => m UGen
    forest_sounds_m = clone 2 insects_m
in uid_st_eval forest_sounds_m
