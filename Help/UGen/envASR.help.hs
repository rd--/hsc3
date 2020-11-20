-- envASR ; random release time
let g = setResetFF 1 (dust 'α' KR 1)
    p = envASR 0.01 1 1 (EnvNum (-4))
    e = envGen KR g 0.1 0 1 RemoveSynth p
in sinOsc AR 440 0 * e

-- envASR ; control
let g = control KR "env-gate" 1
    p = envASR 0.01 1 1 (EnvNum (-4))
    e = envGen KR g 0.1 0 1 RemoveSynth p
in sinOsc AR 440 0 * e

---- ; close gate message
withSC3 (Sound.OSC.sendMessage (n_set1 (-1) "env-gate" 0))

---- ; help
Sound.SC3.Lang.Help.viewSC3Help "Env.*asr"
:i Sound.SC3.ASR

---- ; drawings
import Sound.SC3.Plot {- hsc3-plot -}
plotEnvelope [envASR 0.1 1 1 (EnvNum (-4))
             ,envASR 0.3 0.25 1 EnvSin
             ,envASR 0.01 0.5 1.25 EnvLin]

---- ; drawings ; an envelope with a long release time that mostly sustains and then decays quickly
plotEnvelope [envASR_c 0.01 1 0.75 (EnvNum (-4),EnvNum 4)
             ,envASR_c 0.15 1 1.25 (EnvNum (-4),EnvNum (-4))
             ,envASR_c 0.15 1 1.25 (EnvNum (-4),EnvNum 64)]

