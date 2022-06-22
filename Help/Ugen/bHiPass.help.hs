-- bHiPass
let i = pinkNoiseId 'α' ar
    f = mouseX kr 10 20000 Exponential 0.2
    rq = mouseY kr 0 1 Linear 0.2
in bHiPass i f rq * 0.05

-- bHiPass ; calculate coefficients and use sos (see also bHiPass4)
let i = pinkNoiseId 'α' ar
    f = mouseX kr 10 20000 Exponential 0.2
    rq = mouseY kr 0 1 Linear 0.2
    (a0, a1, a2, b1, b2) = Sound.Sc3.Common.Math.Filter.Beq.bHiPassCoef sampleRate f rq
in sos i a0 a1 a2 b1 b2 * 0.05
