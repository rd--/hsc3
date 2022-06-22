-- bBandPass ; warning=feedback
let i = soundIn 0
    f = mouseX kr 20 20000 Exponential 0.2
    bw = mouseY kr 0 10 Linear 0.2
in bBandPass i f bw

-- bBandPass ; calculate coefficients and use sos ; warning=feedback
let i = soundIn 0
    f = mouseX kr 20 20000 Exponential 0.2
    bw = mouseY kr 0 10 Linear 0.2
    (a0, a1, a2, b1, b2) = Sound.Sc3.Common.Math.Filter.Beq.bBandPassCoef sampleRate f bw
in sos i a0 a1 a2 b1 b2
