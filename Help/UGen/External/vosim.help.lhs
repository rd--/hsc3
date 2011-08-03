vosim tr freq nCycles decay

      trig - starts a vosim pulse when a transition from
             non-positive to positive occurs and no other
             vosim is still going.  a-rate will produce
             sample accurate triggering.

      freq - sets the frequency of the squared sinewave.

   nCycles - sets the number of squared sinewaves to use
             in one vosim pulse.  nCycles gets checked
             when VOSIM receives a trigger.

     decay - sets the decay factor.

> import Sound.SC3.Monadic

> do { p <- tRand 0 1 (impulse AR 6 0)
>    ; let { t = impulse AR (9 * ( 1 + ( p >* 0.95))) 0
>          ; x = mouseX' KR 0.25 2 Linear 0.2
>          ; y = mouseY' KR 0.25 1.5 Linear 0.2
>          ; z = 9
>          ; rng l r i = linLin i (-1) 1 l r
>          ; mk_n = lfNoise2 KR z >>= return . rng 0.25 2
>          ; tR l r = tRand (mce l) (mce r) }
>      in do { f <- tR [40, 120, 220] [440, 990, 880] t
>            ; n <- tR [4] [8, 16, 32] t
>            ; d <- tR [0.2, 0.4, 0.6] [0.6, 0.8, 1] t
>            ; a <- tR [0] [0.2, 0.6, 1] t
>            ; l <- tR [-1] [1] t
>            ; xn <- mk_n
>            ; yn <- mk_n
>            ; let v = vosim t (f * x * xn) n (d * y * yn) * a
>              in audition (out 0 (pan2 (mix v) l 1)) } }
