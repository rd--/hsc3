-- envTriangle
let t = envTriangle 1 0.1
    e = envGen kr 1 1 0 1 RemoveSynth t
in sinOsc ar 440 0 * e

---- ; drawings
Sound.Sc3.Plot.plotEnvelope [envTriangle 1 1,envTriangle 0.25 0.5]

---- ; help
Sound.Sc3.Lang.Help.viewSc3Help "Env.*triangle"
