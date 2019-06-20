> import Sound.SC3 {- hsc3 -}

optimized summation (see sum_opt), ie. Sum3

> g_01 = mix (mce [pinkNoise 'α' AR,fSinOsc AR 801 0,lfSaw AR 40 0]) * 0.1

and Sum4

> g_02 = mix (sinOsc AR (mce (take 10 (iterate (* 2) 36))) 0) * 0.05
