-- swept resonant noise (jmcc) #2 ; texture=overlap,4,5,5,inf
let flt src z =
      let p = 10
          spec = klankSpec_mce
                 (X.linRandN p z 80 10080 0)
                 (mce (replicate p 1))
                 (X.randN p z 0.5 2.5)
      in klank src 1 0 1 spec
    n = whiteNoise 'α' AR * 0.007
    f = midiCPS (fSinOsc KR (rand 'β' 0.1 0.3) 0 * rand 'γ' 0 24 + rand 'δ' 36 84)
    sw = resonz n f 0.1
in mce (map (flt sw) ['ε','ζ'])

-- swept resonant noise (jmcc) #2 ; event control
let f c (g,x,y,z,_,_,_,_,_,_) =
      let flt src k =
            let np = 10
                spec = klankSpec_mce
                  ((x + 0.5) * X.linRandN np (c,k) 80 10080 0)
                  (mce (replicate np 1))
                  (X.randN np (c,k) 0.5 2.5)
            in dynKlank src 1 0 1 spec
          f0 = (0.25 + y) * rand (c,'β') 0.1 0.3
          f1 = midiCPS (fSinOsc KR f0 0 * rand (c,'γ') 0 24 + rand (c,'δ') 36 84)
          sw = resonz (whiteNoise (c,'α') AR * 0.007) f1 0.1
      in mce (map (flt sw) ['ε','ζ']) * z * lag g 0.1
in mix (rEventVoicer 16 f) * control KR "gain" 1
