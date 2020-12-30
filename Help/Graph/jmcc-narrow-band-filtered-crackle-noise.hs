-- narrow band filtered crackle noise (jmcc) #2 ; texture=spawn,2,inf
let e = envLinen 2 5 2 1
    rf1 = rand 'α' 0 2000 + 80
    rf2 = rf1 + (rand2 'β' 0.2 * rf1)
    rf = xLine KR rf1 rf2 9 DoNothing
    c = crackle AR (1.97 + rand 'γ' 0 0.03) * 0.15
in pan2 (resonz c rf 0.2) (rand2 'δ' 1) (envGen AR 1 1 0 1 RemoveSynth e)

-- narrow band filtered crackle noise (jmcc) #2 ; event control
let f c (g,_,y,z,o,_,_,p) =
      let cr = crackle AR (1.97 + rand (c,'γ') 0 0.03)
      in pan2 (resonz cr (midiCPS p) (0.2 - y * 0.2)) (o * 2 - 1) (z * 10 * g)
in mix (rEventVoicer 16 f) * control KR "gain" 1
