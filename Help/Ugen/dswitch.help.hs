-- dswitch ; c.f. dswitch1
uid_st_eval (
  do a0 <- dwhiteM 2 3 4
     a1 <- dwhiteM 2 0 1
     a2 <- dseqM 2 (mce [1,1,1,0])
     i <- dseqM 2 (mce [0,1,2,1,0])
     d <- dswitchM i (mce [a0,a1,a2])
     let t = impulse kr 4 0
         f = demand t 0 d * 300 + 400
     return (sinOsc ar f 0 * 0.1))
