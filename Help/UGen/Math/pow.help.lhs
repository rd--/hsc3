> Sound.SC3.UGen.Help.viewSC3Help "Operator.**"
> :t (**)

> import Sound.SC3.Monadic

> let a = fSinOsc AR 100 0 * 0.1
> in audition (out 0 (mce2 a (a ** 10)))

http://create.ucsb.edu/pipermail/sc-users/2006-December/029998.html
> do { n0 <- lfNoise2 KR 8
>    ; n1 <- lfNoise2 KR 3
>    ; let { s = blip AR (n0 * 200 + 300) (n1 * 10 + 20)
>          ; x = mouseX KR 1000 (sampleRate * 0.5) Exponential 0.1
>          ; y = mouseY KR 1 24 Exponential 0.1
>          ; d = latch s (impulse AR x 0)
>          ; b = roundUp d (0.5 ** y) }
>      in audition (out 0 (mce2 d b)) }
