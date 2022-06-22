-- blip
blip ar 440 200 * 0.1

-- blip ; modulate frequency
let f = xLine kr 20000 200 6 RemoveSynth in blip ar f 100 * 0.1

-- blip ; modulate number of harmonics
let nh = line kr 1 100 20 RemoveSynth in blip ar 200 nh * 0.2

-- blip ; self-modulation at control rate
let fr = blip kr 0.25 3 * 300 + 500
    nh = blip kr 0.15 2 * 20 + 21
in blip ar fr nh * 0.2

-- blip ; event control
let f (_,g,x,y,z,o,_,_,_,_,_) = pan2 (blip ar (midiCps (x * 13 + 48)) (y * 10 + 1)) (o * 2 - 1) (g * z)
in mix (voicer 16 f) * control kr "gain" 0.5

-- blip ; event control (p)
let f (_,g,_,_,z,o,rx,_,p,px,_) =
      let f0 = midiCps (p * 127 + px)
          nh = max 0 rx * 10 + 1
      in pan2 (blip ar f0 nh) (o * 2 - 1) (g * z)
in mix (voicer 16 f) * control kr "gain" 0.5

---- ; drawings
Sound.Sc3.Plot.plot_ugen1 0.1 (blip ar 1000 20)
Sound.Sc3.Plot.FFT.plot_ugen_fft1 0.1 (blip ar 1000 20)
