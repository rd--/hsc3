-- bLowPass ; warning=feedback
let i = soundIn (mce2 0 1)
    f = mouseX KR 10 20000 Exponential 0.2
    rq = mouseY KR 0 1 Linear 0.2
in bLowPass i f rq

-- bLowPass
let i = mix (saw AR (mce [0.99, 1, 1.01] * 440) * 0.05)
    cf = mouseX KR 100 20000 Exponential 0.2 -- center-frequency
    rq = mouseY KR 0.1 1 Linear 0.2 -- reciprocal of Q
in bLowPass i cf rq

-- bLowPass ; calculate coefficients and use sos (see also bLowPass4)
let i = mix (saw AR (mce [0.99, 1, 1.01] * 440) * 0.05)
    cf = mouseX KR 100 20000 Exponential 0.2
    rq = mouseY KR 0.1 1 Linear 0.2
    (a0, a1, a2, b1, b2) = Sound.SC3.Common.Math.Filter.BEQ.bLowPassCoef sampleRate cf rq
in sos i a0 a1 a2 b1 b2

-- bLowPass ; modulate center-frequency
bLowPass (whiteNoise 'α' AR) (xLine KR 24000 20 10 DoNothing) 1 * 0.05

-- bLowPass ; modulate reciprocal of Q
bLowPass (whiteNoise 'α' AR) 1200 (xLine KR 0.5 100 10 DoNothing) * 0.05
