-- analog bubbles with mouse control (jmcc) #3
let y = mouseY KR 0.1 10 Exponential 0.2 {- lfo 1 rate -}
    x = mouseX KR 2 40 Exponential 0.2  {- lfo 2 rate -}
    o2 = lfSaw KR x 0 * (-3) + 80 {- depth & offset in semitones -}
    o1 = lfSaw KR y 0 * 24 + o2 {- depth in semitones, offset is lfo_2 -}
    f = midiCPS o1 {- convert to frequency -}
    s = sinOsc AR f 0 * 0.04
in combN s 0.2 0.2 4 {- echoing sine wave -}

-- analog bubbles (jmcc) #1 ; C-cC-v c.f. hsc3-ui-region
let f1 = control_m KR "f1" 0.4 (0.1,16,"exp")
    f2 = control_m KR "f2" 8.0 (0.1,16,"exp")
    dt = control_m KR "dt" 0.90375 (0.5,2,"lin")
    o = lfSaw KR (mce2 f2 (f2 * dt)) 0 * 3 + 80
    f = lfSaw KR f1 0 * 24 + o
    s = sinOsc AR (midiCPS f) 0 * 0.04
in combN s 0.2 0.2 4
