-- analog bubbles with mouse control (jmcc) #3
let y = mouseY kr 0.1 10 Exponential 0.2 {- lfo 1 rate -}
    x = mouseX kr 2 40 Exponential 0.2  {- lfo 2 rate -}
    o2 = lfSaw kr x 0 * (-3) + 80 {- depth & offset in semitones -}
    o1 = lfSaw kr y 0 * 24 + o2 {- depth in semitones, offset is lfo_2 -}
    f = midiCps o1 {- convert to frequency -}
    s = sinOsc ar f 0 * 0.04
in combN s 0.2 0.2 4 {- echoing sine wave -}

-- analog bubbles (jmcc) #1 ; C-cC-v c.f. hsc3-ui-region
let f1 = control_m kr "f1" 0.4 (0.1,16,"exp")
    f2 = control_m kr "f2" 8.0 (0.1,16,"exp")
    dt = control_m kr "dt" 0.90375 (0.5,2,"lin")
    o = lfSaw kr (mce2 f2 (f2 * dt)) 0 * 3 + 80
    f = lfSaw kr f1 0 * 24 + o
    s = sinOsc ar (midiCps f) 0 * 0.04
in combN s 0.2 0.2 4
