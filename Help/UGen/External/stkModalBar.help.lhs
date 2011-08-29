stkModalBar freq instrument stickHardness stickPosition
            vibratoGain vibratoFreq directStickMix volume trig

  Marimba=0, Vibraphone=1, Agogo=2, Wood1=3, Reso=4,
  Wood2=5, Beats=6, Two Fixed=7, Clump=8

> import Control.Monad
> import Sound.SC3.Monadic

> let { x = mouseX' KR 0.25 4 Linear 0.2
>     ; tr = impulse KR x 0 - 0.5
>     ; tR = tRand 0 127 tr }
> in do { i <- tRand 0 9 tr
>       ; mn <- tIRand 25 96 tr
>       ; [sh, sp, vg, vf, mx, v] <- replicateM 6 tR
>       ; audition (out 0 (stkModalBar AR (midiCPS mn) i sh sp vg vf mx v tr)) }

> let { x = mouseX' KR 1 6 Linear 0.2
>     ; t = impulse KR x 0 - 0.5
>     ; tr = pulseDivider t 6 0 }
> in do { mn <- tIRand 52 64 t
>       ; sh <- tRand 4 8 tr
>       ; sp <- tRand 54 68 tr
>       ; vg <- tRand 66 98 tr
>       ; vf <- tRand 4 12 tr
>       ; mx <- tRand 0 1 tr
>       ; v <- tRand 16 48 tr
>       ; audition (out 0 (stkModalBar AR (midiCPS mn) 1 sh sp vg vf mx v t)) }
