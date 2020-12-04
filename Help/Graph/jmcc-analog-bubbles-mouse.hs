-- analog bubbles with mouse control (jmcc) #3
let y = mouseY KR 0.1 10 Exponential 0.2 {- lfo 1 rate -}
    x = mouseX KR 2 40 Exponential 0.2  {- lfo 2 rate -}
    o2 = lfSaw KR x 0 * (-3) + 80 {- depth & offset in semitones -}
    o1 = lfSaw KR y 0 * 24 + o2 {- depth in semitones, offset is lfo_2 -}
    f = midiCPS o1 {- convert to frequency -}
    s = sinOsc AR f 0 * 0.04
in combN s 0.2 0.2 4 {- echoing sine wave -}
