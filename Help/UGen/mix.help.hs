-- mix ; optimized summation (see sum_opt), ie. Sum3
mix (mce [pinkNoise 'α' AR,fSinOsc AR 801 0,lfSaw AR 40 0]) * 0.05

-- mix ; c.f. sum4
mix (sinOsc AR (mce (take 10 (iterate (* 2) 36))) 0) * 0.025
