amClip a b

0 when b <= 0, a*b when b > 0

> n <- whiteNoise AR
> audition (out 0 (amClip n (fSinOsc KR 1 0 * 0.2)))
