-- blip
blip AR 440 200 * 0.1

-- blip ; modulate frequency
let f = xLine KR 20000 200 6 RemoveSynth in blip AR f 100 * 0.1

-- blip ; modulate number of harmonics
let nh = line KR 1 100 20 RemoveSynth in blip AR 200 nh * 0.2

-- blip ; self-modulation at control rate
let fr = blip KR 0.25 3 * 300 + 500
    nh = blip KR 0.15 2 * 20 + 21
in blip AR fr nh * 0.2

-- blip ; event control
let f _ (g,_,y,z,o,_,_,p) = pan2 (blip AR (midiCPS p) (y * 10 + 1)) (o * 2 - 1) (g * z)
in mix (rEventVoicer 10 f) * control KR "gain" 0.5

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.1 (blip AR 1000 20)
Sound.SC3.Plot.FFT.plot_ugen_fft1 0.1 (blip AR 1000 20)
