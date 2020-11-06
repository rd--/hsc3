-- bHiPass
let i = pinkNoise 'α' AR
    f = mouseX KR 10 20000 Exponential 0.2
    rq = mouseY KR 0 1 Linear 0.2
in bHiPass i f rq * 0.05

-- bHiPass ; calculate coefficients and use sos (see also bHiPass4)
let i = pinkNoise 'α' AR
    f = mouseX KR 10 20000 Exponential 0.2
    rq = mouseY KR 0 1 Linear 0.2
    (a0, a1, a2, b1, b2) = Sound.SC3.Common.Math.Filter.BEQ.bHiPassCoef sampleRate f rq
in sos i a0 a1 a2 b1 b2 * 0.05
