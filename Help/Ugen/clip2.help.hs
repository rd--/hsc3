-- clip2 ; clipping distortion
clip2 (fSinOsc ar 400 0) 0.01

-- clip2
clip2 (fSinOsc ar 400 0) (line kr 0 1 8 RemoveSynth) * 0.1

---- ; drawings
Sound.Sc3.Plot.plot_fn_r1_ln (\x -> clip2 x 1) (-2,2)
