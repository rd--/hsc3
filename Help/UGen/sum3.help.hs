-- sum3
sum3 (sinOsc AR 440 0) (sinOsc AR 441 0) (sinOsc AR 442 0) * 0.1

-- sum3 ; optimiser/re-writer applicable at ADD
(sinOsc AR 440 0 + sinOsc AR 441 0 + sinOsc AR 442 0) * 0.1

-- sum3
mix (sinOsc AR (mce [440 .. 442]) 0) * 0.1
