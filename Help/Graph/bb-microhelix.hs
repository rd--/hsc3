-- batuhan bozkurt 2009 http://www.earslap.com (bb) - mce...........................
let rp = replicate
    ctrigs_ () =
        let i = mce (rp 8 (1/8) ++
                     rp 8 (1/4) ++
                     [dseq (drand dinf (mce2 1 2)) (mce2 (1/16) (1/16))])
            d = dxrand dinf (mceMap (* 1.25) i)
        in tDuty ar d 0 DoNothing (dwhite dinf 0.5 1) 0
    ctrigs = mce2 (ctrigs_ ()) (ctrigs_ ())
    clicks =
        let n = pinkNoise ar * decay ctrigs (0.001 * abs (lfNoise1 ar 4))
        in fold (bpf n 15000 0.9 * (25 * range 0 1 (lfNoise1 ar 8))) (-1) 1
    snd1 =
        let o = sinOsc ar (midiCps 44) 0 * 0.5 + sinOsc ar (midiCps 90) 0 * 0.6
        in lpf o (midiCps 32) * 2 + hpf (lpf (whiteNoise ar * 0.008) 12000) 2400
    hiNoise =
        let n = whiteNoise ar
            e = decay2 (ctrigs * (lfNoise1 ar 8 * 0.5 + 0.5)) 0.02 0.1 * 0.05
        in bpf (n * e) (tRand 12000 15000 ctrigs) 0.9
    trigMod = roundTo (lfNoise0 ar 8) 1
    bass =
        let t = mceChannel 0 ctrigs * trigMod
            ph = sweep ar t (2 * pi * mce2 52.8 740) + (pi/3)
            ph' = wrap ph (-pi) pi
            mean u = sum (mceChannels u) / fromIntegral (mceDegree_err u)
            o = tanh (mean (sinOsc ar 0 ph' * mce2 2 0.05))
            d = Envelope [0, 0.5, 0.4, 0] [0, 0.2, 0.01] [EnvNum (-5)] Nothing Nothing 0
            e = envGen ar (abs t) 1 0 1 DoNothing d
        in o * e * 0.25
    snd1' =
        let t = mceChannel 0 ctrigs
            d = Envelope [0, 1, 0.6, 0] [0.0001, 0.4, 0.01] [EnvNum (-4)] Nothing Nothing 0
            e = envGen ar (t * lfNoise0 ar 8) 1 0 1 DoNothing d
        in pan2 (snd1 * e) (tRand (-1) 1 t) 1
in limiter (midEQ (clicks + snd1' + hiNoise + bass) 14000 0.7 8) 1 0.01

-- batuhan bozkurt 2009 http://www.earslap.com (bb) - mce...........................
let rp = replicate
    ctrigs_ z =
        let i = mce (rp 8 (1/8) ++
                     rp 8 (1/4) ++
                     [dseqId (z,'α') (drandId (z,'β') dinf (mce2 1 2)) (mce2 (1/16) (1/16))])
            d = dxrandId (z,'γ') dinf (mceMap (* 1.25) i)
        in tDuty ar d 0 DoNothing (dwhiteId (z,'δ') dinf 0.5 1) 0
    ctrigs = mce2 (ctrigs_ 'a') (ctrigs_ 'a')
    clicks =
        let n = pinkNoiseId 'ε' ar * decay ctrigs (0.001 * abs (lfNoise1Id 'ζ' ar 4))
        in fold (bpf n 15000 0.9 * (25 * range 0 1 (lfNoise1Id 'η' ar 8))) (-1) 1
    snd1 =
        let o = sinOsc ar (midiCps 44) 0 * 0.5 + sinOsc ar (midiCps 90) 0 * 0.6
        in lpf o (midiCps 32) * 2 + hpf (lpf (whiteNoiseId 'θ' ar * 0.008) 12000) 2400
    hiNoise =
        let n = whiteNoiseId 'ι' ar
            e = decay2 (ctrigs * (lfNoise1Id 'κ' ar 8 * 0.5 + 0.5)) 0.02 0.1 * 0.05
        in bpf (n * e) (tRandId 'λ' 12000 15000 ctrigs) 0.9
    trigMod = roundTo (lfNoise0Id 'μ' ar 8) 1
    bass =
        let t = mceChannel 0 ctrigs * trigMod
            ph = sweep ar t (2 * pi * mce2 52.8 740) + (pi/3)
            ph' = wrap ph (-pi) pi
            mean u = sum (mceChannels u) / fromIntegral (mceDegree_err u)
            o = tanh (mean (sinOsc ar 0 ph' * mce2 2 0.05))
            d = Envelope [0, 0.5, 0.4, 0] [0, 0.2, 0.01] [EnvNum (-5)] Nothing Nothing 0
            e = envGen ar (abs t) 1 0 1 DoNothing d
        in o * e * 0.25
    snd1' =
        let t = mceChannel 0 ctrigs
            d = Envelope [0, 1, 0.6, 0] [0.0001, 0.4, 0.01] [EnvNum (-4)] Nothing Nothing 0
            e = envGen ar (t * lfNoise0Id 'ν' ar 8) 1 0 1 DoNothing d
        in pan2 (snd1 * e) (tRandId 'ξ' (-1) 1 t) 1
in limiter (midEQ (clicks + snd1' + hiNoise + bass) 14000 0.7 8) 1 0.01
