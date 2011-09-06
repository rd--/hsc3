> Sound.SC3.UGen.Help.viewSC3Help "DegreeToKey"
> Sound.SC3.UGen.DB.ugenSummary "DegreeToKey"

> import Sound.SC3.ID

allocate & initialise buffer zero
> withSC3 (\fd -> async fd (b_alloc_setn1 0 0 [0,2,3.2,5,7,9,10]))

modal space, mouse x controls discrete pitch in dorian mode
> let {n = lfNoise1 'a' KR (mce [3,3.05])
>     ;x = mouseX' KR 0 15 Linear 0.1
>     ;k = degreeToKey 0 x 12
>     ;f b = let {o = sinOsc AR (midiCPS (b + k + n * 0.04)) 0 * 0.1
>                ;t = lfPulse AR (midiCPS (mce [48,55])) 0.15 0.5
>                ;d = rlpf t (midiCPS (sinOsc KR 0.1 0 * 10 + b)) 0.1 * 0.1
>                ;m = o + d}
>            in combN m 0.31 0.31 2 + m}
> in audition (out 0 ((f 48 + f 72) * 0.25))
