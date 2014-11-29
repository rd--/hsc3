-- Sound.SC3.UGen.Help.viewSC3Help "Integrator"
-- Sound.SC3.UGen.DB.ugenSummary "Integrator"

import Sound.SC3

-- > audition (out 0 h1)
h1 =
    let x = mouseX KR 0.001 0.999 Exponential 0.2
        o = lfPulse AR 300 0.2 0.1 * 0.1
    in integrator o x

-- used as an envelope
--
-- > audition (out 0 h2)
h2 =
    let i = lfPulse AR 3 0.2 0.0004
        o = sinOsc AR 700 0
    in integrator i 0.999 * o


-- scope
--
-- > audition (out 0 h3)
h3 =
    let x = mouseX KR 0.01 0.999 Exponential 0.2
        o = lfPulse AR (1500 / 4) 0.2 0.1
    in integrator o x * 0.1
