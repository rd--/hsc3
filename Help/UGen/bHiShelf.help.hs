-- bHiShelf ; warning=feedback
let i = soundIn 0
    f = mouseX KR 2200 18000 Exponential 0.2
    db = mouseY KR 18 (-18) Linear 0.2
in bHiShelf i f 1 db * 0.25

-- bHiShelf ; warning=feedback
let i = soundIn 0
    f = mouseX KR 2200 18000 Exponential 0.2
    rs = mouseY KR 0.1 1 Linear 0.2
in bHiShelf i f rs 6 * 0.25

-- bHiShelf ; calculate coefficients and use sos ; warning=feedback
let i = soundIn 0
    f = mouseX KR 2200 18000 Exponential 0.2
    rs = mouseY KR 0.1 1 Linear 0.2
    (a0, a1, a2, b1, b2) = Sound.SC3.Common.Math.Filter.BEQ.bHiShelfCoef sampleRate f rs 6
in sos i a0 a1 a2 b1 b2
