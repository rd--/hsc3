-- bPeakEQ
let i = pinkNoise 'α' AR * 0.1
    freq = mouseX KR 2200 18000 Exponential 0.2
    db = mouseY KR 12 (-12) Linear 0.2
in bPeakEQ i freq 0.8 db * 0.5

-- bPeakEQ
let i = pinkNoise 'α' AR * 0.1
    freq = mouseX KR 2200 18000 Exponential 0.2
    rq = mouseY KR 10 0.4 Linear 0.2
in bPeakEQ i freq rq 6 * 0.5

-- bPeakEQ ; calculate coefficients and use sos (see also bLowPass4)
let i = pinkNoise 'α' AR * 0.1
    freq = mouseX KR 2200 18000 Exponential 0.2
    rq = mouseY KR 10 0.4 Linear 0.2
    (a0, a1, a2, b1, b2) = Sound.SC3.Common.Math.Filter.BEQ.bPeakEQCoef sampleRate freq rq 6
in sos i a0 a1 a2 b1 b2 * 0.5

-- bPeakEQ ; parametric
let hf z = bPeakEQ
           z
           (control_m KR "hfFreq" 4000 (800,20000,"exp"))
           (control_m KR "hfRQ" 1 (0.01,2,"lin"))
           (control_m KR "hfGain" 0 (-15,15,"lin"))
    mf z = bPeakEQ
           z
           (control_m KR "mfFreq" 1000 (200,5000,"exp"))
           (control_m KR "mfRQ" 1 (0.01,2,"lin"))
           (control_m KR "mfGain" 0 (-15,15,"lin"))
    lf z = bPeakEQ
           z
           (control_m KR "lfFreq" 100 (20,500,"exp"))
           (control_m KR "lfRQ" 1 (0.01,2,"lin"))
           (control_m KR "lfGain" 0 (-15,15,"lin"))
    sig = pinkNoise 'α' AR * 0.1
in hf (mf (lf sig))
