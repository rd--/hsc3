splay in spread level center

splay spreads an array of channels across the stereo field.

spread -    0 = mono, 1 = stereo
level  -    0 = silent, 1 = unit gain (equal power level compensated)
center -   -1 = left, 1 = right

> do { i <- return 6
>    ; r <- replicateM i (rand 10 20)
>    ; n <- lfNoise2 KR (mce r)
>    ; let { ci = constant . fromIntegral
>          ; x = mouseX KR (-1) 1 Linear 0.1
>          ; y = mouseY KR 1 0 Linear 0.1
>          ; o = sinOsc AR (n * 200 + (mce [1 .. ci i] + 3 * 100)) 0 }
>      in audition (out 0 (splay o y 0.2 x)) }
