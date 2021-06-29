-- runningMin ; follow a sine lfo, reset rate controlled by mouseX
let o = sinOsc kr 2 pi
    x = mouseX kr 0.01 10 Exponential 0.1
    t = impulse ar x 0
    f = runningMin o t * 500 + 200
in t + sinOsc ar f 0 * 0.1

-- runningMin
let n = dust 'α' ar 20
    t = impulse ar 0.4 0
    f = runningMin n t * 500 + 200
in sinOsc ar f 0 * 0.2
