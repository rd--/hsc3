-- narrow band filtered crackle noise (jmcc) #2 ; texture=spawn,2,inf
let e = envLinen 2 5 2 1
    rf1 = rand 0 2000 + 80
    rf2 = rf1 + (rand2 0.2 * rf1)
    rf = xLine kr rf1 rf2 9 DoNothing
    c = crackle ar (1.97 + rand 0 0.03) * 0.15
in pan2 (resonz c rf 0.2) (rand2 1) (envGen ar 1 1 0 1 RemoveSynth e)

-- narrow band filtered crackle noise (jmcc) #2 ; texture=spawn,2,inf ; id
let e = envLinen 2 5 2 1
    rf1 = randId 'α' 0 2000 + 80
    rf2 = rf1 + (rand2Id 'β' 0.2 * rf1)
    rf = xLine kr rf1 rf2 9 DoNothing
    c = crackle ar (1.97 + randId 'γ' 0 0.03) * 0.15
in pan2 (resonz c rf 0.2) (rand2Id 'δ' 1) (envGen ar 1 1 0 1 RemoveSynth e)

-- narrow band filtered crackle noise (jmcc) #2 ; event control
let f _ (g,_,y,z,o,_,_,p,_,_) =
      let cr = crackle ar (1.97 + rand 0 0.03)
      in pan2 (resonz cr (midiCps p) (0.2 - y * 0.2)) (o * 2 - 1) (z * 10 * g)
in mix (eventVoicer 16 f) * control kr "gain" 1

-- narrow band filtered crackle noise (jmcc) #2 ; event control ; id
let f c (g,_,y,z,o,_,_,p,_,_) =
      let cr = crackle ar (1.97 + randId (c,'γ') 0 0.03)
      in pan2 (resonz cr (midiCps p) (0.2 - y * 0.2)) (o * 2 - 1) (z * 10 * g)
in mix (eventVoicer 16 f) * control kr "gain" 1
