    Sound.SC3.UGen.Help.viewSC3Help "Latch"
    Sound.SC3.UGen.DB.ugenSummary "Latch"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let n = whiteNoise 'α' AR
>       i = impulse AR 9 0
>       l = latch n i
>   in blip AR (l * 400 + 500) 4 * 0.2

The above is just meant as example. LFNoise0 is a faster way to
generate random steps :

> g_02 =
>   let n = lfNoise0 'α' KR 9
>   in blip AR (n * 400 + 500) 4 * 0.2

http://create.ucsb.edu/pipermail/sc-users/2006-December/029991.html

> g_03 =
>   let n0 = lfNoise2 'α' KR 8
>       n1 = lfNoise2 'β' KR 3
>       s = blip AR (n0 * 200 + 300) (n1 * 10 + 20)
>       x = mouseX KR 1000 (sampleRate * 0.1) Exponential 0.1
>   in latch s (impulse AR x 0)
