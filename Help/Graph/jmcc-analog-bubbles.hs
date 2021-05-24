-- analog bubbles (jmcc) #1
let o = lfSaw KR (mce2 8 7.23) 0 * 3 + 80
    f = lfSaw KR 0.4 0 * 24 + o
    s = sinOsc AR (midiCPS f) 0 * 0.04
in combN s 0.2 0.2 4 * 0.1
