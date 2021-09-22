-- https://sccode.org/1-590 (dm)
let shift_register_f n tr x =
      let buf = localBuf 1 n
          count = pulseCount tr 0
      in mrg2
         (demand tr 0 (mceReverse (dbufrd buf (mceMap (+ count) (mce [1 .. n])) Loop)))
         (demand tr 0 (dbufwr buf count x Loop))
    amp = 0.1
    ip = impulse kr (1/16) 0
    rt = tChoose ip (mce [3,5,10])
    trs = tChoose ip (mce [0,2,-2,7,-5])
    tr1 = trig1 (cuspL ar (rt * 3) 1 1.9 0.0) 0.001
    tr4 = pulseDivider tr1 4 0
    oct = demand tr4 0 (drand dinf (mce [12,-12]))
    note = demand tr1 0 (dseq dinf (mce [42,46,51,54,59,63,66] + oct + trs)) -- scramble
    chord = shift_register_f 5 tr1 (midiCps note)
    sig = pmOsc ar
          (vibrato ar chord 6 0.02 0 0 0.04 0.1 0.0 0.0)
          (urange 1.01 2.01 (lfPulse kr (1/8) 0 0.5) * chord)
          (envGen kr tr1 1 0 1 DoNothing (envelope [3,3,0] [0, 0.2] [EnvNum (-4)]))
          0
    cmp = mix (sig * ampCompA kr chord 0 0.32 1 * amp)
in xFade2
   (mce2 cmp cmp)
   (gVerb (bpf cmp (midiCps 90) 1) 50 8 0.5 0.5 15 0 0.7 0.5 300)
   0.2
   1

-- https://sccode.org/1-590 (dm) ; id
let shift_register_f n tr x =
      let buf = localBufId 'α' 1 n
          count = pulseCount tr 0
      in mrg2
         (demand tr 0 (mceReverse (dbufrdId 'γ' buf (mceMap (+ count) (mce [1 .. n])) Loop)))
         (demand tr 0 (dbufwrId 'β' buf count x Loop))
    amp = 0.1
    ip = impulse kr (1/16) 0
    rt = tChooseId 'δ' ip (mce [3,5,10])
    trs = tChooseId 'ε' ip (mce [0,2,-2,7,-5])
    tr1 = trig1 (cuspL ar (rt * 3) 1 1.9 0.0) 0.001
    tr4 = pulseDivider tr1 4 0
    oct = demand tr4 0 (drandId 'ζ' dinf (mce [12,-12]))
    note = demand tr1 0 (dseqId 'η' dinf (mceMap (+ trs) (mce [42,46,51,54,59,63,66])) + oct) -- scramble
    chord = shift_register_f 5 tr1 (midiCps note)
    sig = pmOsc ar
          (vibratoId 'θ' ar chord 6 0.02 0 0 0.04 0.1 0.0 0.0)
          (urange 1.01 2.01 (lfPulse kr (1/8) 0 0.5) * chord)
          (envGen kr tr1 1 0 1 DoNothing (envelope [3,3,0] [0, 0.2] [EnvNum (-4)]))
          0
    cmp = mix (sig * ampCompA kr chord 0 0.32 1 * amp)
in xFade2
   (mce2 cmp cmp)
   (gVerb (bpf cmp (midiCps 90) 1) 50 8 0.5 0.5 15 0 0.7 0.5 300)
   0.2
   1
