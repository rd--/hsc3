-- swept resonant noise (jmcc) #2 ; texture=overlap,4,5,5,inf
let flt src _ =
      let p = 10
          spec = klankSpec_mce
                 (X.rLinRandN p 80 10080 0)
                 (mce (replicate p 1))
                 (X.rRandN p 0.5 2.5)
      in klank src 1 0 1 spec
    n = whiteNoise ar * 0.007
    f = midiCps (fSinOsc kr (rand 0.1 0.3) 0 * rand 0 24 + rand 36 84)
    sw = resonz n f 0.1
in mceFill 2 (flt sw)

-- swept resonant noise (jmcc) #2 ; texture=overlap,4,5,5,inf ; id
let flt src z =
      let p = 10
          spec = klankSpec_mce
                 (X.rLinRandNId p z 80 10080 0)
                 (mce (replicate p 1))
                 (X.rRandNId p z 0.5 2.5)
      in klank src 1 0 1 spec
    n = whiteNoiseId 'α' ar * 0.007
    f = midiCps (fSinOsc kr (randId 'β' 0.1 0.3) 0 * randId 'γ' 0 24 + randId 'δ' 36 84)
    sw = resonz n f 0.1
in mce (map (flt sw) ['ε','ζ'])

-- swept resonant noise (jmcc) #2 ; event control
let f _ (g,x,y,z,_,_,_,_,_,_) =
      let flt src _ =
            let np = 10
                spec = klankSpec_mce
                  ((x + 0.5) * X.rLinRandN np 80 10080 0)
                  (mce (replicate np 1))
                  (X.rRandN np 0.5 2.5)
            in dynKlank src 1 0 1 spec
          f0 = (0.25 + y) * rand 0.1 0.3
          f1 = midiCps (fSinOsc kr f0 0 * rand 0 24 + rand 36 84)
          sw = resonz (whiteNoise ar * 0.007) f1 0.1
      in mceFill 2 (flt sw) * z * lag g 0.1
in mix (eventVoicer 16 f) * control kr "gain" 1

-- swept resonant noise (jmcc) #2 ; event control ; id
let f c (g,x,y,z,_,_,_,_,_,_) =
      let flt src k =
            let np = 10
                spec = klankSpec_mce
                  ((x + 0.5) * X.rLinRandNId np (c,k) 80 10080 0)
                  (mce (replicate np 1))
                  (X.rRandNId np (c,k) 0.5 2.5)
            in dynKlank src 1 0 1 spec
          f0 = (0.25 + y) * randId (c,'β') 0.1 0.3
          f1 = midiCps (fSinOsc kr f0 0 * randId (c,'γ') 0 24 + randId (c,'δ') 36 84)
          sw = resonz (whiteNoiseId (c,'α') ar * 0.007) f1 0.1
      in mce (map (flt sw) ['ε','ζ']) * z * lag g 0.1
in mix (eventVoicer 16 f) * control kr "gain" 1
