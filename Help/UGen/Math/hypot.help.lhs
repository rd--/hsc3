> Sound.SC3.UGen.Help.viewSC3Help "hypot"
> :t hypot

> import Sound.SC3

> let { x = mouseX' KR 0 0.1 Linear 0.1
>     ; y = mouseY' KR 0 0.1 Linear 0.1 }
> in audition (out 0 (sinOsc AR 440 0 * hypot x y))

Object travels 200 meters in 6 secs (=120kph) passing 10 meters
from the listener.  The speed of sound is 344 meters/sec.
> let { x = 10
>     ; y = lfSaw KR (1 / 6) 0 * 100
>     ; d = hypot x y
>     ; v = slope d
>     ; r = (344 - v) / 344
>     ; a = 10 / (squared d) }
> in audition (out 0 (fSinOsc AR (1000 * r) 0 * a))

> let { x = 10
>     ; y = lfSaw KR (1 / 6) 0 * 100
>     ; d = hypot x y
>     ; a = 40 / (squared d)
>     ; s = rlpf (fSinOsc AR 200 0 * lfPulse AR 31.3 0 0.4) 400 0.3 }
> in audition (out 0 (delayL s (110 / 344) (d / 344) * a))
