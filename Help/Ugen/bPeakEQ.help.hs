-- bPeakEQ
let i = pinkNoiseId 'α' ar * 0.1
    freq = mouseX kr 2200 18000 Exponential 0.2
    db = mouseY kr (-12) 12 Linear 0.2
in bPeakEQ i freq 0.8 db * 0.5

-- bPeakEQ
let i = pinkNoiseId 'α' ar * 0.1
    freq = mouseX kr 2200 18000 Exponential 0.2
    rq = mouseY kr 10 0.4 Linear 0.2
in bPeakEQ i freq rq 6 * 0.5

-- bPeakEQ ; calculate coefficients and use sos (see also bLowPass4)
let i = pinkNoiseId 'α' ar * 0.1
    freq = mouseX kr 2200 18000 Exponential 0.2
    rq = mouseY kr 10 0.4 Linear 0.2
    (a0, a1, a2, b1, b2) = Sound.Sc3.Common.Math.Filter.Beq.bPeakEQCoef sampleRate freq rq 6
in sos i a0 a1 a2 b1 b2 * 0.5

-- bPeakEQ ; parametric
let hf z = bPeakEQ
           z
           (control_m kr "hfFreq" 4000 (800,20000,"exp"))
           (control_m kr "hfRQ" 1 (0.01,2,"lin"))
           (control_m kr "hfGain" 0 (-15,15,"lin"))
    mf z = bPeakEQ
           z
           (control_m kr "mfFreq" 1000 (200,5000,"exp"))
           (control_m kr "mfRQ" 1 (0.01,2,"lin"))
           (control_m kr "mfGain" 0 (-15,15,"lin"))
    lf z = bPeakEQ
           z
           (control_m kr "lfFreq" 100 (20,500,"exp"))
           (control_m kr "lfRQ" 1 (0.01,2,"lin"))
           (control_m kr "lfGain" 0 (-15,15,"lin"))
    sig = pinkNoiseId 'α' ar * 0.1
in hf (mf (lf sig))

---- ; drawings
UI.ui_sc3_scope_freq (600,400) 0
