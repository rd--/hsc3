-- mix ; optimized summation (see sum_opt), ie. Sum3
mix (mce [pinkNoiseId 'α' ar,fSinOsc ar 801 0,lfSaw ar 40 0]) * 0.05

-- mix ; c.f. sum4
mix (sinOsc ar (mce (take 10 (iterate (* 2) 36))) 0) * 0.025

-- mix ; mixing stereo signals (mce)
mix (mce [sinOsc ar (mce2 330 990) 0 * 0.05,sinOsc ar (mce2 331 991) 0 * 0.05])

-- mix ; mixing stereo signals (mce) ; sums left with left and right with right
mix (mce2 (sinOsc ar (mce2 440 441) 0) (sinOsc ar (mce2 220 221) 0)) * 0.1

-- mix ; mixing stereo signals (pan) ; sums left with left and right with right
mix (mce2 (pan2 (sinOsc ar 440 0) (-1) 0.1) (pan2 (sinOsc ar 441 0) 1 0.1))

-- mix ; phase cancellation
mix (mce2 (sinOsc ar 440 0) (sinOsc ar 440 pi))

-- mix ; channel layout is L=440,441 and R=660,661
let f = mce2 (mce2 440 660) (mce2 441 661)
in mix (sinOsc ar f 0 * 0.1)
