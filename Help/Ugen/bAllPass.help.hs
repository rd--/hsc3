-- bAllPass ; thoughpass ; warning=feedback
let i = soundIn (mce2 0 1)
    f = mouseX kr 10 18000 Exponential 0.2
in bAllPass i f 0.8 * 0.2

-- bAllPass ; bandpass ; warning=feedback
let i = soundIn (mce2 0 1) * 0.5
    f = mouseX kr 100 18000 Exponential 0.2
in bAllPass i f 0.8 + negate i

-- bAllPass ; calculate coefficients and use sos ; warning=feedback
let i = soundIn (mce2 0 1) * 0.5
    f = mouseX kr 100 18000 Exponential 0.2
    (a0, a1, a2, b1, b2) = Sound.Sc3.Common.Math.Filter.Beq.bAllPassCoef sampleRate f 0.8
in sos i a0 a1 a2 b1 b2 + negate i
