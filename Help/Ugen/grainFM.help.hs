-- grainFM
let d = 15
    lin a b = line kr a b d RemoveSynth
    l = lin (-0.5) 0.5
    f = lin 200 800
    t = impulse kr (lin 7.5 15) 0
    i = lin (-1) 1
in grainFM 2 t 0.1 f 200 i l (-1) 512 * 0.1

-- grainFM ; mouse control
let n1 = whiteNoiseId 'α' kr
    n2 = lfNoise1Id 'β' kr 500
    x = mouseX kr (-0.5) 0.5 Linear 0.1
    y = mouseY kr 0 400 Linear 0.1
    f = n1 * y + 440
    t = impulse kr 12.5 0
    i = linLin n2 (-1) 1 1 10
in grainFM 2 t 0.1 f 200 i x (-1) 512 * 0.1

-- grainFM ; event control
let f (_,g,x,y,z,o,rx,ry,p,px,_) =
      let tr = impulse ar (y * 64 + 10) 0
          cf = midiCps (p * 127 + px)
          mf = (cf * 1.5) + ((1 - x) * z * cf)
      in grainFM 2 tr (ry * 0.25) cf mf (1 + (rx * 0.25)) o (-1) 512 * z * lagUD g 0 2
in mix (voicer 16 f) * control kr "gain" 0.5
