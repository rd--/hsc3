-- bLowShelf ; warning=feedback
let i = soundIn (mce2 0 1)
    freq = mouseX kr 40 6000 Exponential 0.2
    rs = 1
    db = mouseY kr 24 (-24) Linear 0.2
in bLowShelf i freq rs db * 0.5

-- bLowShelf ; warning=feedback
let i = soundIn (mce2 0 1)
    freq = mouseX kr 20 6000 Exponential 0.2
    rs = mouseY kr 0.1 1 Linear 0.2
    db = 6
in bLowShelf i freq rs db * 0.5

-- bLowShelf ; calculate coefficients and use sos ; warning=feedback
let i = soundIn (mce2 0 1)
    freq = mouseX kr 20 6000 Exponential 0.2
    rs = mouseY kr 0.1 1 Linear 0.2
    db = 6
    (a0, a1, a2, b1, b2) = Sound.Sc3.Common.Math.Filter.Beq.bLowShelfCoef sampleRate freq rs db
in sos i a0 a1 a2 b1 b2 * 0.5
