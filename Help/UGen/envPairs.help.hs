-- envPairs
let c = EnvLin
    p = envPairs [(0,0),(5,0.01),(5.5,0.1),(10,0)] c
    e = envGen KR 1 1 0 1 RemoveSynth p
in sinOsc AR 440 0 * e

---- ; help
Sound.SC3.Lang.Help.viewSC3Help "Env.*pairs"
