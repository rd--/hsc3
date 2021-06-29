-- analog bubbles (jmcc) #1
let o = lfSaw kr (mce2 8 7.23) 0 * 3 + 80
    m = lfSaw kr 0.4 0 * 24 + o
    s = sinOsc ar (midiCPS m) 0 * 0.04
in combN s 0.2 0.2 4 * 0.1
