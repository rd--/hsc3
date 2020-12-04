-- ; berlin 1977 (jmcc) #4
let dsequ z s tr = demand tr 0 (dseq z dinf (mce s))
    dsequR z s tr = demand tr 0 (dshuf z dinf (mce s))
    clock_rate = mouseX KR 5 20 Exponential 0.2 {- mouse x controls clock rate -}
    clock_time = 1 / clock_rate
    clock = impulse KR clock_rate 0 {- sequencer trigger -}
    patternList = [55,60,63,62,60,67,63,58]
    note = dsequ 'α' patternList clock {- midi note pattern sequencer -}
    clock_16 = pulseDivider clock 16 0 {- divide clock by 16 -}
    note' = dsequR 'β' [-12,-7,-5,0,2,5] clock_16 + note {- transpose somewhat randomly -}
    freq = midiCPS note' {- convert midi note to cycles per second -}
    env = decay2 clock (0.05 * clock_time) (2 * clock_time)
    amp = env * 0.1 + 0.02 {- amplitude envelope -}
    filt = env * (fSinOsc KR 0.17 0 * 800) + 1400 {- filter frequency -}
    pw = sinOsc KR 0.08 (mce2 0 (0.5 * pi)) * 0.45 + 0.5 {- pulse width LFO(s) -}
    s = pulse AR freq pw * amp
in combN (rlpf s filt 0.15) 0.2 (mce2 0.2 0.17) 1.5
