-- trig1 ; timed gate, duration in seconds
let d = dustId 'α' ar 1
    o = fSinOsc ar 800 0 * 0.1
in o * trig1 d 1.25

-- trig1
sinOsc ar 440 0 * trig1 (impulse kr 10 0) 0.1 * 0.1

-- trig1 ; if duration is set to zero it is reset to one frame (at ar & kr) ; 3.11.2
let env = decay2 (trig1 (mce2 (sinOsc kr 10 0) (sinOsc ar 9 0)) 0) 0.01 0.2
in sinOsc ar (mce2 440 441) 0 * env * 0.1
