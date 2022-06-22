-- envSine
let s = envSine 9 0.1
    e = envGen kr 1 1 0 1 RemoveSynth s
in sinOsc ar 440 0 * e

---- ; drawings
Sound.Sc3.Plot.plotEnvelope [envSine 9 1,envSine 3 0.25]

---- ; help
Sound.Sc3.Lang.Help.viewSc3Help "Env.*sine"
