-- pulsing bottles (jmcc) #2 ; texture=overlap,4,4,4,maxBound
let r z = let n = whiteNoise z AR
              r0 = rand z 4 14
              r1 = rand z 0 0.7
              r2 = rand z 400 7400
          in resonz (n * lfPulse KR r0 0 0.25 * r1) r2 0.01
    s z = let f = rand z 0.1 0.5
              p = rand z 0 (pi * 2)
          in sinOsc KR f p
  in sum (zipWith3 pan2 (map r (id_seq 6 'α')) (map s (id_seq 6 'β')) (repeat 1))
