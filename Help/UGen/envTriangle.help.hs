-- envTriangle
let t = envTriangle 1 0.1
    e = envGen kr 1 1 0 1 RemoveSynth t
in sinOsc ar 440 0 * e

---- ; drawings
Sound.SC3.Plot.plotEnvelope [envTriangle 1 1,envTriangle 0.25 0.5]

---- ; help
Sound.SC3.Lang.Help.viewSC3Help "Env.*triangle"
