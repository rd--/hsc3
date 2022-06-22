-- formant ; default values
formant ar 440 1760 880 * 0.125

-- formant ; modulate fundamental frequency, formant frequency stays constant
formant ar (xLine kr 400 1000 8 RemoveSynth) 2000 800 * 0.125

-- formant ; modulate formant frequency, fundamental frequency stays constant
let f = mce [200, 300, 400, 500]
    ff = xLine kr 400 4000 8 RemoveSynth
in splay (formant ar f ff 200) 1 1 0 True * 0.125

-- formant ; modulate width frequency, other frequencies stay constant
let bw = xLine kr 800 8000 8 RemoveSynth
in formant ar 400 2000 bw * 0.1

-- formant ; event control
let f (_,g,x,y,z,o,_,_,_,_,_) =
      let f0 = mce [200, 300, 400, 500] * x
          ff = linExp y 0 1 400 1200
      in splay (formant ar f0 ff 200) 1 (g * z) (o * 2 - 1) True
in mix (voicer 16 f) * control kr "gain" 0.5
