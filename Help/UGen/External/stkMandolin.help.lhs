stkMandolin rate f bs pp dm dt at tr

        f - frequency
       bs - body size
       pp - pick position
       dm - string damping
       dt - string detune
       at - after touch

> let { x = mouseX KR 0.25 4 Linear 0.2 
>     ; tr = impulse KR x 0 - 0.5 }
> in do { mn <- tRand 54 66 tr
>       ; [bs, pp, dm, dt, at] <- replicateM 5 (tRand 0 127 tr)
>       ; audition (out 0 (stkMandolin AR (midiCPS mn) bs pp dm dt at tr)) }

> let { x = mouseX KR 3 16 Linear 0.2 
>     ; t = impulse KR x 0 - 0.5 
>     ; tr = pulseDivider t 6 0 }
> in do { mn <- tiRand 54 66 t
>       ; bs <- tRand 72 94 tr
>       ; pp <- tRand 32 42 tr
>       ; dm <- tRand 64 72 tr
>       ; dt <- tRand 0 4 tr
>       ; at <- tRand 2 8 tr
>       ; audition (out 0 (stkMandolin AR (midiCPS mn) bs pp dm dt at t)) }
