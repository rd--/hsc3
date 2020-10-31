-- sum4
sum4 (sinOsc AR 440 0) (sinOsc AR 441 0) (sinOsc AR 442 0) (sinOsc AR 443 0) * 0.1

-- sum4 ; optimiser/re-writer applicable at ADD
mix (sinOsc AR (mce [440 .. 443]) 0) * 0.1
