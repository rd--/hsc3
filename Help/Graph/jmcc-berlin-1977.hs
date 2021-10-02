-- berlin 1977 (jmcc) #4
let dsequ sq tr = demand tr 0 (dseq dinf (mce sq))
    dsequR sq tr = demand tr 0 (dshuf dinf (mce sq))
    clock_rate = mouseX kr 5 20 Exponential 0.2 {- mouse x controls clock rate -}
    clock_time = 1 / clock_rate
    clock = impulse kr clock_rate 0 {- sequencer trigger -}
    patternList = [55,60,63,62,60,67,63,58]
    note = dsequ patternList clock {- midi note pattern sequencer -}
    clock_16 = pulseDivider clock 16 0 {- divide clock by 16 -}
    note' = dsequR [-12,-7,-5,0,2,5] clock_16 + note {- transpose somewhat randomly -}
    freq = midiCps note' {- convert midi note to cycles per second -}
    env = decay2 clock (0.05 * clock_time) (2 * clock_time)
    amp = env * 0.1 + 0.02 {- amplitude envelope -}
    filt = env * (fSinOsc kr 0.17 0 * 800) + 1400 {- filter frequency -}
    pw = sinOsc kr 0.08 (mce2 0 (0.5 * pi)) * 0.45 + 0.5 {- pulse width LFO(s) -}
    s = pulse ar freq pw * amp
in combN (rlpf s filt 0.15) 0.2 (mce2 0.2 0.17) 1.5

-- berlin 1977 (jmcc) #4 ; id
let dsequId z sq tr = demand tr 0 (dseqId z dinf (mce sq))
    dsequRId z sq tr = demand tr 0 (dshufId z dinf (mce sq))
    clock_rate = mouseX kr 5 20 Exponential 0.2 {- mouse x controls clock rate -}
    clock_time = 1 / clock_rate
    clock = impulse kr clock_rate 0 {- sequencer trigger -}
    patternList = [55,60,63,62,60,67,63,58]
    note = dsequId 'α' patternList clock {- midi note pattern sequencer -}
    clock_16 = pulseDivider clock 16 0 {- divide clock by 16 -}
    note' = dsequRId 'β' [-12,-7,-5,0,2,5] clock_16 + note {- transpose somewhat randomly -}
    freq = midiCps note' {- convert midi note to cycles per second -}
    env = decay2 clock (0.05 * clock_time) (2 * clock_time)
    amp = env * 0.1 + 0.02 {- amplitude envelope -}
    filt = env * (fSinOsc kr 0.17 0 * 800) + 1400 {- filter frequency -}
    pw = sinOsc kr 0.08 (mce2 0 (0.5 * pi)) * 0.45 + 0.5 {- pulse width LFO(s) -}
    s = pulse ar freq pw * amp
in combN (rlpf s filt 0.15) 0.2 (mce2 0.2 0.17) 1.5

-- berlin 1977 (jmcc) #4 ; event-control
let f (_,g,_,y,z,_,_,_,p,_,_) =
      let freq = midiCps p
          env = decay2 (trig g controlDur) (0.05 * y) (2 * y)
          amp = env * z + 0.02
          filt = env * (fSinOsc kr 0.17 0 * 800) + 1400 {- filter frequency -}
          pw = sinOsc kr 0.08 (mce2 0 (0.5 * pi)) * 0.45 + 0.5 {- pulse width LFO(s) -}
          s = pulse ar freq pw * amp
      in combN (rlpf s filt 0.15) 0.2 (mce2 0.2 0.17) 1.5 * lagUD g 0 (2 + y)
in mix (eventVoicer 16 f) * control kr "gain" 1

