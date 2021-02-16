-- trig
let d = dust 'α' AR 1
    o = fSinOsc AR 800 0 * 0.5
in o * trig d 0.2

-- trig ; if duration is set to zero it is reset to one frame (at AR & KR) ; 3.11.2
let env = decay2 (trig (mce2 (sinOsc KR 10 0 * 10) (sinOsc AR 9 0 * 1000)) 0) 0.01 0.2
in sinOsc AR (mce2 440 441) 0 * env * 0.1
