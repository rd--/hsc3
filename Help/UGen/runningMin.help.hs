-- runningMin ; follow a sine lfo, reset rate controlled by mouseX
let o = sinOsc KR 2 pi
    x = mouseX KR 0.01 10 Exponential 0.1
    t = impulse AR x 0
    f = runningMin o t * 500 + 200
in t + sinOsc AR f 0 * 0.1

-- runningMin
let n = dust 'α' AR 20
    t = impulse AR 0.4 0
    f = runningMin n t * 500 + 200
in sinOsc AR f 0 * 0.2
