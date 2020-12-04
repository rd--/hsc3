-- pv_RandWipe
let z1 = let n0 = randomRs (400.0, 1000.0) (mkStdGen 0)
             o0 = map (\n -> lfSaw AR n 0 * 0.1) (take 6 n0)
         in sum_opt o0
    z2 = let n1 = randomRs (80.0, 400.0) (mkStdGen 1)
             n2 = randomRs (0.0, 8.0) (mkStdGen 2)
             o1 = map (\n -> lfPulse AR n 0.0 0.2) (take 6 n1)
             o2 = map (\n -> sinOsc KR n 0 * 0.2) (take 6 n2)
         in sum_opt (zipWith (\p s -> p * (max s 0.0)) o1 o2)
    f1 = fft' (localBuf 'α' 2048 1) z1
    f2 = fft' (localBuf 'β' 2048 1) z2
    x = mouseX KR 0 1 Linear 0.1
    y = mouseY KR 0 1 Linear 0.1
    h = pv_RandWipe 'γ' f1 f2 x (y `greater_than` 0.5)
in pan2 (ifft' h) 0 0.5
