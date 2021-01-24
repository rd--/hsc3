-- grainSin ; mouse control
let n = whiteNoise 'α' KR
    x = mouseX KR (-0.5) 0.5 Linear 0.1
    y = mouseY KR 0 400 Linear 0.1
    f = n * y + 440
    t = impulse KR 10 0
in grainSin 2 t 0.1 f x (-1) 512 * 0.1

-- grainSin ; f0 https://www.listarc.bham.ac.uk/lists/sc-users/msg66911.html
let k = 16
    t = impulse AR (mouseY KR 1 999 Linear 0.2) 0
    f i = ((fromIntegral i ** range_hs (0.3,0.7) (lfNoise0 i KR 1)) + 1) * 99
    l = mce (map f [0::Int .. k - 1])
in grainSin 2 t (mouseX KR 0.001 0.5 Exponential 0.2) (tChoose 'α' t l) 0 (-1) 512 * 0.1

-- grainSin ; event control
let f _ (g,_,y,z,o,rx,ry,p) =
      let tr = impulse AR (linLin y 0 1 6 72) 0
          du = linLin rx 0 1 0.01 0.15
      in grainSin 2 tr du (midiCPS (p + (ry * 2 - 1))) (o * 2 - 1) (-1) 512 * z * g
in mix (rEventVoicer 16 f) * control KR "gain" 1
