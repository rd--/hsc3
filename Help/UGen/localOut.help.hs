-- localOut ; resonator, must subtract blockSize for correct tuning
let p = localIn 1 ar 0
    i = impulse ar 1 0
    d = delayC (i + (p * 0.995)) 1 (recip 440 - recip controlRate)
in mrg [offsetOut 0 p,localOut d]

-- localOut ; compare with oscillator
sinOsc ar 440 0 * 0.2

-- localOut ; ping pong
let n = decay (impulse ar 0.3 0) 0.1 * whiteNoise 'α' ar * 0.2
    l = localIn 2 ar 0 + mce2 n 0
    d = delayN l 0.2 0.2
    o = localOut (mceReverse d * 0.8)
in mrg2 d o
