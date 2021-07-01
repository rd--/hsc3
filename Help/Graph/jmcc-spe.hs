-- spe (jmcc)
let rapf i = allpassN i 0.05 (X.rRandN 2 0 0.05) 4
    src = let t = impulse kr 9 0
              e = envGen kr t 0.1 0 1 DoNothing (envPerc 0.1 1)
              s = mce [00,03,02,07
                      ,08,32,16,18
                      ,00,12,24,32]
              f = midiCPS (demand t 0 (dseq dinf s) + 32)
              rq = midiCPS (lfNoise1 kr 1 * 36 + 110)
          in rlpf (lfSaw ar f 0 * e) rq 0.1
in iter 4 rapf src

-- spe (jmcc) ; monad
uid_st_eval (do
  let rapf i = do r <- replicateM 2 (randM 0 0.05)
                  return (allpassN i 0.05 (mce r) 4)
      src = do let t = impulse kr 9 0
                   e = envGen kr t 0.1 0 1 DoNothing (envPerc 0.1 1)
                   s = mce [00,03,02,07
                           ,08,32,16,18
                           ,00,12,24,32]
               n <- lfNoise1M kr 1
               m <- dseqM dinf s
               let f = midiCPS (demand t 0 m + 32)
                   o = lfSaw ar f 0 * e
                   rq = midiCPS (n * 36 + 110)
               return (rlpf o rq 0.1)
  chainM 4 rapf =<< src)
