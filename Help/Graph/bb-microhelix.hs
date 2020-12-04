-- batuhan bozkurt 2009 http://www.earslap.com (bb) - mce...........................
let rp = replicate
    offset_id :: Enum a => Int -> a -> Int
    offset_id n e = n + fromEnum e
    ctrigs_ z_ =
        let z = offset_id z_
            i = mce (rp 8 (1/8) ++
                     rp 8 (1/4) ++
                     [dseq (z 'α') (drand (z 'β') dinf (mce2 1 2)) (mce2 (1/16) (1/16))])
            d = dxrand (z 'γ') dinf (mceMap (* 1.25) i)
        in tDuty AR d 0 DoNothing (dwhite (z 'δ') dinf 0.5 1) 0
    ctrigs = mce2 (ctrigs_ 0) (ctrigs_ 12)
    clicks =
        let n = pinkNoise 'ε' AR * decay ctrigs (0.001 * abs (lfNoise1 'ζ' AR 4))
        in fold (bpf n 15000 0.9 * (25 * range 0 1 (lfNoise1 'η' AR 8))) (-1) 1
    snd1 =
        let o = sinOsc AR (midiCPS 44) 0 * 0.5 + sinOsc AR (midiCPS 90) 0 * 0.6
        in lpf o (midiCPS 32) * 2 + hpf (lpf (whiteNoise 'θ' AR * 0.008) 12000) 2400
    hiNoise =
        let n = whiteNoise 'ι' AR
            e = decay2 (ctrigs * (lfNoise1 'κ' AR 8 * 0.5 + 0.5)) 0.02 0.1 * 0.05
        in bpf (n * e) (tRand 'λ' 12000 15000 ctrigs) 0.9
    trigMod = roundTo (lfNoise0 'μ' AR 8) 1
    bass =
        let t = mceChannel 0 ctrigs * trigMod
            ph = sweep t (2 * pi * mce2 52.8 740) + (pi/3)
            ph' = wrap ph (-pi) pi
            mean u = sum (mceChannels u) / fromIntegral (mceDegree_err u)
            o = tanh (mean (sinOsc AR 0 ph' * mce2 2 0.05))
            d = Envelope [0, 0.5, 0.4, 0] [0, 0.2, 0.01] [EnvNum (-5)] Nothing Nothing 0
            e = envGen AR (abs t) 1 0 1 DoNothing d
        in o * e * 0.25
    snd1' =
        let t = mceChannel 0 ctrigs
            d = Envelope [0, 1, 0.6, 0] [0.0001, 0.4, 0.01] [EnvNum (-4)] Nothing Nothing 0
            e = envGen AR (t * lfNoise0 'ν' AR 8) 1 0 1 DoNothing d
        in pan2 (snd1 * e) (tRand 'ξ' (-1) 1 t) 1
in limiter (midEQ (clicks + snd1' + hiNoise + bass) 14000 0.7 8) 1 0.01
