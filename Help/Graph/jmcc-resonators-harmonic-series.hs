-- resonators harmonic series (jmcc) #2 ; texture=xfade,1,7,inf
let nharm = 12
    spec _ = let rat = [1.0,1.125,1.25,1.333,1.5,1.667,1.875,2.0,2.25,2.5,2.667,3.0,3.333,3.75,4.0]
                 freq = lchoose rat * 120
                 series n i j = case n of {0 -> []; _ -> i : series (n - 1) (i + j) j}
             in klankSpec_mce
                (mce (series nharm freq freq) + X.randN nharm (-0.5) 0.5)
                (mceFill nharm (\i -> 1 / (i + 1)))
                (X.randN nharm 0.5 4.5)
    gen n = klank (brownNoise ar * 0.001) 1 0 1 (spec n)
in mceFill 2 gen

-- resonators harmonic series (jmcc) #2 ; texture=xfade,1,7,inf ; id
let series n i j = case n of {0 -> []; _ -> i : series (n - 1) (i + j) j}
    enumFromN e i = let j = fromEnum e in [j .. j + i]
    nharm = 12
    noise = brownNoiseId 'α' ar * 0.001
    rat = [1.0,1.125,1.25,1.333,1.5,1.667,1.875,2.0,2.25,2.5,2.667,3.0,3.333,3.75,4.0]
    freq = lchooseId 'β' rat * 120
    resFreqs = zipWith (+)
               (series nharm freq freq)
               (map (\z -> rand2Id ('γ',z) 0.5) (enumFromN 'δ' nharm))
    spec = klankSpec
           resFreqs
           (map (\i -> 1 / (constant i + 1)) [0 .. nharm - 1])
           (map (\z -> randId ('ε',z) 0.5 4.5) (enumFromN 'ζ' nharm))
in Protect.uclone_all 'η' 2 (klank noise 1 0 1 spec)

-- resonators harmonic series (jmcc) #2 ; event control
let f (_,g,_,y,z,o,rx,_,p,_,_) =
      let nharm = 12
          spec _ = let freq = unitCps p
                       series n i j = case n of {0 -> []; _ -> i : series (n - 1) (i + j) j}
                   in klankSpec_mce
                      (mce (series nharm freq freq) + (X.randN nharm (-0.5) 0.5) * rx)
                      (mceFill nharm (\i -> 1 / (i + (2 * y))))
                      (X.randN nharm 0.5 4.5)
          gen n = dynKlank (brownNoise ar * 0.001) 1 0 1 (spec n)
      in pan2 (mceFill 2 gen)  (o * 2 - 1) (z * g)
in mix (eventVoicer 16 f) * control kr "gain" 1

-- resonators harmonic series (jmcc) #2 ; event control ; id
let f (c,g,_,y,z,o,rx,ry,p,_,_) =
      let series n i j = case n of {0 -> []; _ -> i : series (n - 1) (i + j) j}
          enumFromN e i = let j = fromEnum e in [j .. j + i]
          nharm = 12
          noise = brownNoiseId (c,'α') ar * 0.001
          freq = unitCps p
          resFreqs = zipWith (+)
                     (series nharm freq freq)
                     (map (\k -> rand2Id (c,'γ',k) 0.5 * rx) (enumFromN 'δ' nharm))
          spec = klankSpec
            resFreqs
            (map (\i -> 1 / (constant i + (2 * y))) [0 .. nharm - 1])
            (map (\k -> randId (c,'ε',k) 0.5 4.5 * (0.5 + ry)) (enumFromN 'ζ' nharm))
      in pan2 (Protect.uclone_all 'η' 2 (dynKlank noise 1 0 1 spec)) (o * 2 - 1) (z * g)
in mix (eventVoicer 16 f) * control kr "gain" 1
