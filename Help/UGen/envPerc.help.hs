-- envPerc
let a = 0.1
    p = envPerc 0.01 1
    e = envGen KR 1 a 0 1 RemoveSynth p
in sinOsc AR 440 0 * e

-- envPerc
let a = 0.1
    c = EnvNum (-4)
    p = envPerc_c 0.01 1 a (c,c)
    e = envGen KR 1 1 0 1 RemoveSynth p
in sinOsc AR 440 0 * e

---- ; drawings
Sound.SC3.Plot.plotEnvelope [envPerc 0.05 1,envPerc 0.2 0.75]
Sound.SC3.Plot.plotEnvelope [envPerc 0.1 1,envPerc_c 0.1 1 1 (EnvSin,EnvSin)]

---- ; help
Sound.SC3.UGen.Help.viewSC3Help "Env.*perc"

---- ; coordinates
envelope_sc3_array (envPerc 0.01 1) == Just [0,2,-99,-99,1,0.01,5,-4,0,1,5,-4]
