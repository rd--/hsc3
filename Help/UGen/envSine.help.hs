-- envSine
let s = envSine 9 0.1
    e = envGen KR 1 1 0 1 RemoveSynth s
in sinOsc AR 440 0 * e

---- ; drawings
Sound.SC3.Plot.plotEnvelope [envSine 9 1,envSine 3 0.25]

---- ; help
Sound.SC3.Lang.Help.viewSC3Help "Env.*sine"
