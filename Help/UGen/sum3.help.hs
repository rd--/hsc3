-- sum3
sum3 (sinOsc ar 440 0) (sinOsc ar 441 0) (sinOsc ar 442 0) * 0.1

-- sum3 ; optimiser/re-writer applicable at ADD
(sinOsc ar 440 0 + sinOsc ar 441 0 + sinOsc ar 442 0) * 0.1

-- sum3
mix (sinOsc ar (mce [440 .. 442]) 0) * 0.1
