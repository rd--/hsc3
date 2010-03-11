tipnso (rd)

> import Sound.SC3.Monadic

> main =
>   let { x = mouseX kr 1 32 Linear 0.1
>       ; t = impulse ar x 0 }
>   in do { n1 <- tiRand 16 72 t
>         ; n2 <- tiRand 0 1 t
>         ; n3 <- pinkNoise ar
>         ; let { e = decay2 t 0.01 (mce2 0.1 0.15)
>               ; f = midiCPS (n1 + 36 + (12 * n2))
>               ; s = sinOsc ar f 0 * e
>               ; b = bpf (n3 * e) (36 + midiCPS n1) (175 / (midiCPS n1)) }
>           in audition (out 0 ((s + b) * mce2 0.15 0.1)) }
