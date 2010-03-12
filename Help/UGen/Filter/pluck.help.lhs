pluck in tr maxdelaytime delaytime decaytime coef

Karplus-Strong synthesis.

in - an excitation signal

tr - upon a negative to positive transition, the excitation signal
     will be fed into the delay line

maxdelaytime - the max delay time in seconds (initializes the
               internal delay buffer).

delaytime - delay time in seconds.

decaytime - time for the echoes to decay by 60 decibels. Negative
            times emphasize odd partials.

coef - the coef of the internal OnePole filter. Values should be
       between -1 and +1 (larger values will be unstable... so be
       careful!).

Excitation signal is WhiteNoise, triggered twice a second with
varying OnePole coef.

> import Sound.SC3.Monadic

> do { n <- whiteNoise AR
>    ; let { t = impulse KR 9 0
>          ; x = mouseX KR (-0.999) 0.999 Linear 0.1
>          ; y = mouseY KR 0.1 1 Linear 0.1
>          ; dl = 1 / 440 }
>      in audition (out 0 (pluck (n * 0.25) t dl (dl * y) 10 x)) }

> let n = 25
> in do { f <- clone n (rand 0.05 0.2)
>       ; p <- clone n (rand 0 1)
>       ; w <- clone n (whiteNoise AR)
>       ; fi <- clone n (rand 10 12)
>       ; coef <- rand 0.01 0.2
>       ; l <- clone n (rand (-1) 1)
>       ; let { x = mouseX KR 60 1000 Exponential 0.1
>             ; o = linLin (sinOsc KR f p) (-1) 1 x 3000
>             ; i = impulse KR fi 0
>             ; ks = pluck (w * 0.1) i 0.01 (1 / o) 2 coef }
>         in audition (out 0 (leakDC (mix (pan2 ks l 1)) 0.995)) }
