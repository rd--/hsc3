-- envPairs
let c = EnvLin
    p = envPairs [(0,0),(5,0.01),(5.5,0.1),(10,0)] c
    e = envGen kr 1 1 0 1 RemoveSynth p
in sinOsc ar 440 0 * e

---- ; plot
import Sound.SC3.Plot {- hsc3-plot -}
plotEnvelope [envPairs [(0,0),(5,0.01),(5.5,0.1),(10,0)] EnvLin]
plotEnvelope [envPairs [(0,1),(2.1,0.5),(3,1.4),(5,0.001)] EnvExp]
plotEnvelope [envPairs [(0,110),(4,220),(9,440),(11,220),(13,880),(19,110),(23,55),(27,55)] EnvExp]

---- ; help
Sound.SC3.Common.Help.sc3_scdoc_help_open False (sc3_scdoc_help_path "Env.*pairs")
