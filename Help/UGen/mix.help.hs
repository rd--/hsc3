-- mix ; optimized summation (see sum_opt), ie. Sum3
mix (mce [pinkNoise 'α' AR,fSinOsc AR 801 0,lfSaw AR 40 0]) * 0.05

-- mix ; c.f. sum4
mix (sinOsc AR (mce (take 10 (iterate (* 2) 36))) 0) * 0.025

-- mix ; mixing stereo signals ; sums left with left and right with right
mix (mce2 (sinOsc AR (mce2 440 441) 0) (sinOsc AR (mce2 220 221) 0)) * 0.1

-- mix ; mixing stereo signals ; sums left with left and right with right
mix (mce2 (pan2 (sinOsc AR 440 0) (-1) 0.1) (pan2 (sinOsc AR 441 0) 1 0.1))
