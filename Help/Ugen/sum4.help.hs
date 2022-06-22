-- sum4
sum4 (sinOsc ar 440 0) (sinOsc ar 441 0) (sinOsc ar 442 0) (sinOsc ar 443 0) * 0.1

-- sum4 ; optimiser/re-writer applicable at ADD
mix (sinOsc ar (mce [440 .. 443]) 0) * 0.1
