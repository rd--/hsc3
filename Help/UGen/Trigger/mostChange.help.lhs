mostChange a b

Output the input that changed most.

> do { n <- lfNoise0 KR 1
>    ; let { x = mouseX KR 200 300 Linear 0.1
>          ; f = mostChange (n * 400 + 900) x }
>      in audition (out 0 (sinOsc AR f 0 * 0.1)) }
