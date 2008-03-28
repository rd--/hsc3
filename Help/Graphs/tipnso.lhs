tipnso (rd)

> let { x = mouseX KR 1 32 Linear 0.1
>     ; t = impulse AR x 0 }
> in do { n1 <- tiRand 16 72 t
>       ; n2 <- tiRand 0 1 t
>       ; n3 <- pinkNoise AR
>       ; let { e = decay2 t 0.01 (MCE [0.1, 0.15])
>             ; f = midiCPS (n1 + 36 + (12 * n2))
>             ; s = sinOsc AR f 0 * e
>             ; b = bpf (n3 * e) (36 + midiCPS n1) (175 / (midiCPS n1)) }
>         in audition (out 0 ((s + b) * mce2 0.15 0.1)) }
