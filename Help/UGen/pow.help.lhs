    > Sound.SC3.UGen.Help.viewSC3Help "Operator.**"
    > :t (**)

> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let a = fSinOsc AR 100 0 * 0.1
>     in mce2 a (a ** 10)

see also <http://create.ucsb.edu/pipermail/sc-users/2006-December/029998.html>

> g_02 =
>     let n0 = lfNoise2 'α' KR 8
>         n1 = lfNoise2 'β' KR 3
>         s = blip AR (n0 * 200 + 300) (n1 * 10 + 20)
>         x = mouseX KR 1000 (sampleRate * 0.5) Exponential 0.1
>         y = mouseY KR 1 24 Exponential 0.1
>         d = latch s (impulse AR x 0)
>         b = roundUp d (0.5 ** y)
>     in mce2 d b
