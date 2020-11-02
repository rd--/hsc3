-- brf
let i = saw AR 200 * 0.1
    freq = fSinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3800 + 4000
    rq = 0.3
in brf i freq rq
