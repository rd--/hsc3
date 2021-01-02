-- strummable silk (jmcc) #11
let strummable_guitar_str (ix,z) =
      let n = 15
          k = constant ix
          x = mouseX KR 0 1 Linear 0.2
          tr = abs (hpz1 (x `greater_than` (0.25 + k * 0.07)))
          e = decay (impulse AR 14 0 * lag (trig tr 1) 0.2 * 0.01) 0.04
          pluck = pinkNoise z AR * e
          freq0 = midiCPS (([-2,0,3,5,7,10,12,15] !! ix) + 60)
          freq = mce (map ((*) freq0) [1 .. n + 1])
          param = klankSpec_mce freq (mce (replicate n 1)) (X.randN n z 0.3 1)
      in pan2 (klank pluck 1 0 1 param) (k * 0.2 - 0.5) 1
    strs = sum (map strummable_guitar_str (zip [0..7] ['α'..]))
    r_allpass i = allpassN i 0.1 (X.randN 2 'ζ' 0 0.05) 4
    rev = Protect.useq_all 'λ' 6 r_allpass
in rev (leakDC (lpf strs 12000) 0.995)

-- strummable silk (jmcc) #11 ; event control
let f c (g,_,y,z,o,_,_,p) =
      let n = 15
          e = decay (impulse AR (linExp y 0 1 11 19) 0 * (z * 2 + lag (trig g 1) 0.2) * 0.04) 0.04
          plk = pinkNoise c AR * e
          freq = mce (map ((*) (midiCPS p)) [1 .. n + 1])
          param = klankSpec_mce freq (mce (replicate n 1)) (X.randN n c 0.3 1)
      in pan2 (dynKlank plk 1 0 1 param) (o * 2 - 1) z
    r_allpass i = allpassN i 0.1 (X.randN 2 'ζ' 0 0.05) 4
    rev = Protect.useq_all 'λ' 6 r_allpass
in rev (leakDC (lpf (mix (rEventVoicer 16 f)) 12000) 0.995) * control KR "gain" 1