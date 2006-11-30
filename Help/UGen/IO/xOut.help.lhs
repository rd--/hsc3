xOut bufferIndex xFade inputs
 
Send signal to a bus, crossfading with existing contents.

> let p a b = sinOsc AR (MCE [a, b]) 0 * 0.1
>     x     = mouseX KR 0 1 Linear 0.1
>     y     = mouseY KR 0 1 Linear 0.1
> audition $ MRG [out  0 (p 220 221),
>                 xout 0 x (p 330 331),
>                 xout 0 y (p 440 441),
>                 out  0 (p 120 121)]
