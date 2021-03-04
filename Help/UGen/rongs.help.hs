-- rongs
let tr_freq = mouseX KR 2 24 Linear 0.1
    tr = dust2 'α' KR tr_freq
    sig = let f0 = mce2 1.0 1.01 * tRand 'β' 0.01 0.2 tr
              structure = tRand 'γ' 0.0 0.99 tr
              brightness = tRand 'δ' 0.6 0.99 tr
              stretch = tRand 'ε' 0.1 0.99 tr
              damping = 0.7
              accent = 0.99
              loss = 0.1
          in X.rongs AR tr tr f0 structure brightness damping accent stretch 0.15 loss
in splay (leakDC sig 0.995) 0.25 1 (tRand 'ζ' (-1) 1 tr) True
