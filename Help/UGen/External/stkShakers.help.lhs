stkShakers rt instr energy decay nObjects rfreq tr

     instr - model type
    energy - initial energy
     decay - rate of decay of system
  nObjects - number of particles / elements
     rfreq - resonance frequency
        tr - reset trigger

  Maraca=0, Cabasa=1, Sekere=2, Guiro=3, Water Drops =4,
  Bamboo Chimes=5, Tambourine=6, Sleigh Bells=7, Sticks=8,
  Crunch=9, Wrench=10, Sand Paper=11, Coke Can=12, Next
  Mug=13, Penny + Mug=14, Nickle + Mug = 15, Dime + Mug=16,
  Quarter + Mug=17, Franc + Mug=18, Peso + Mug=19, Big
  Rocks=20, Little Rocks=21, Tuned Bamboo Chimes=22

> import Control.Monad
> import Sound.SC3.Monadic

> let { x = mouseX' KR 0.25 4 Linear 0.2
>     ; tr = impulse KR x 0 - 0.5 }
> in do { i <- tRand 0 23 tr
>       ; [e, sd, no, rf] <- replicateM 4 (tRand 0 127 tr)
>       ; audition (out 0 (stkShakers AR i e sd no rf tr)) }

> let tr = impulse KR 1 0 - 0.5
> in audition (out 0 (stkShakers AR 4 64 64 64 64 tr))
