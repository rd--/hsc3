-- rongs
let tr_freq = sinOsc KR 0.1 (pi/2) `in_exprange` (10.1,1.0)
    tr = dust2 'α' KR tr_freq
    sig = let f0 = mce2 1.0 1.01 * tRand 'β' 0.001 0.100 tr
              structure = tRand 'γ' 0.25 0.75 tr
              brightness = tRand 'δ' 0.25 0.75 tr
              damping = tRand 'ζ' 0.15 0.65 tr
              accent = 0.9
              stretch = tRand 'ε' 0.1 0.99 tr
              position = 0.15
              loss = tRand 'η' 0.1 0.5 tr
          in X.rongs AR tr tr f0 structure brightness damping accent stretch position loss
in splay (leakDC sig 0.995) 0.25 1 (tRand 'θ' (-1) 1 tr) True

-- rongs ; event control
let f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let tr = trig1 g controlDur
          f0 = x * 0.125
          structure = 0.5 + ry * tRand 'α' (-1) 1 tr
          brightness = 0.5 + rx * tRand 'β' (-1) 1 tr
          damping = y * 0.25
          accent = 0.99
          stretch = 0.5 + z * tRand 'γ' (-0.5) 0.5 tr
          position = y * 0.5
          loss = 0.25 + o * tRand 'δ' (-0.1) 0.1 tr
          sig = X.rongs AR tr tr f0 structure brightness damping accent stretch position loss
      in pan2 (leakDC sig 0.995) (o * 2 - 1) 1
in mix (rEventVoicer 6 f) * control KR "gain" 1
