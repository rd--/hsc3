-- brf
let i = saw ar 200 * 0.1
    freq = fSinOsc kr (xLine kr 0.7 300 20 RemoveSynth) 0 * 3800 + 4000
    rq = 0.3
in brf i freq rq
