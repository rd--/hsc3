-- decay2 ; used as an envelope
let s = fSinOsc ar 600 0 * 0.25 -- sinOsc ar 11000 0 * 0.25
    f = xLine kr 1 50 20 RemoveSynth
in decay2 (impulse ar f 0) 0.01 0.2 * s

-- decay2 ; compare with decay used as the envelope
let s = fSinOsc ar 600 0 * 0.25
    f = xLine kr 1 50 20 RemoveSynth
in decay (impulse ar f 0) 0.2 * s

---- ; drawings ; attack and decay are a difference of two decays, hence inversion
Sound.Sc3.Plot.plot_ugen1 0.05 (decay2 (impulse ar 1 0) 0.001 0.01)
Sound.Sc3.Plot.plot_ugen1 0.05 (decay2 (impulse ar 1 0) 0.01 0.001)
