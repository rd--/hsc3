-- lpf
let f = xLine kr 0.7 300 20 RemoveSynth
    ff = fSinOsc kr f 0 * 3600 + 4000
in lpf (saw ar 200 * 0.1) ff

-- lpf ; control rate filtering
let ctl = lpf (lfPulse kr 8 0 0.5) (mouseX kr 2 50 Exponential 0.1)
in sinOsc ar (ctl * 200 + 400) 0 * 0.1

-- lpf ; event control
let f (_,g,_,y,z,o,_,_,p,_,_) =
      let f0 = unitCps p
      in pan2 (lpf (blip ar f0 (1 + y * 5)) (f0 * 0.25)) (o * 2 - 1) (lagUD g 0.05 1 * z)
in mix (voicer 16 f) * control kr "gain" 4

-- lpf
let k = 32
    n = mceFill k (\i -> lpf (dust2Id i ar (int_to_ugen i + 1 / 3)) 1500)
in splay n (lfNoise2Id 'α' kr 0.1 `in_range` (0,1)) 1 0 True
