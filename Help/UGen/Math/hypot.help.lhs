hypot x y

Returns the square root of the sum of the squares of a and b. Or
equivalently, the distance from the origin to the point (x, y).

> let x = mouseX KR 0 0.1 Linear 0.1
>     y = mouseY KR 0 0.1 Linear 0.1
> audition $ sinOsc AR 440 0 * hypot x y

Object travels 200 meters in 6 secs (=120kph) passing 10 meters
from the listener.  The speed of sound is 344 meters/sec.

> let x = 10
>     y = lfSaw KR (1 / 6) 0 * 100
>     d = hypot x y
>     v = slope d
>     r = (344 - v) / 344
>     a = 10 / (squared d)
> audition $ fSinOsc AR (1000 * r) 0 * a

> let x = 10
>     y = lfSaw KR (1 / 6) 0 * 100
>     d = hypot x y
>     a = 40 / (squared d)
>     s = rlpf (fSinOsc AR 200 0 * lfPulse AR 31.3 0 0.4) 400 0.3
> audition $ delayL s (110 / 344) (d / 344) * a
