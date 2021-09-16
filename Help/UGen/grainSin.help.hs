-- grainSin ; mouse control
let n = whiteNoiseId 'α' kr
    x = mouseX kr (-0.5) 0.5 Linear 0.1
    y = mouseY kr 0 400 Linear 0.1
    f = n * y + 440
    t = impulse kr 10 0
in grainSin 2 t 0.1 f x (-1) 512 * 0.1

-- grainSin ; f0 https://www.listarc.bham.ac.uk/lists/sc-users/msg66911.html
let k = 16
    t = impulse ar (mouseY kr 1 999 Linear 0.2) 0
    f i = ((fromIntegral i ** range_hs (0.3,0.7) (lfNoise0Id i kr 1)) + 1) * 99
    l = mce (map f [0::Int .. k - 1])
in grainSin 2 t (mouseX kr 0.001 0.5 Exponential 0.2) (tChooseId 'α' t l) 0 (-1) 512 * 0.1

-- grainSin ; event control
let f _ (g,_,y,z,o,rx,ry,p,_,_) =
      let tr = impulse ar (linLin y 0 1 6 72) 0
          du = linLin rx 0 1 0.01 0.15
      in grainSin 2 tr du (midiCps (p + (ry * 2 - 1))) (o * 2 - 1) (-1) 512 * z * g
in mix (eventVoicer 16 f) * control kr "gain" 1

-- grainsin ; mouse control
let overlap = mouseY kr 0 2 Linear 0.2
    f = mouseX kr 1 220 Linear 0.2
in grainSin 2 (impulse ar f 0) (overlap / f) 440 0 (-1) 512 * 0.1
