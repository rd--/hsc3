-- envAsr ; random release time
let g = setResetFF 1 (dustId 'α' kr 1)
    p = envAsr 0.01 1 1 (EnvNum (-4))
    e = envGen kr g 0.1 0 1 RemoveSynth p
in sinOsc ar 440 0 * e

-- envAsr ; control
let g = control kr "env-gate" 1
    p = envAsr 0.01 1 1 (EnvNum (-4))
    e = envGen kr g 0.1 0 1 RemoveSynth p
in sinOsc ar 440 0 * e

---- ; close gate message
withSc3 (Sound.OSC.sendMessage (n_set1 (-1) "env-gate" 0))

---- ; help
Sound.Sc3.Lang.Help.viewSc3Help "Env.*asr"
:i Sound.Sc3.Asr

---- ; drawings
import Sound.Sc3.Plot {- hsc3-plot -}
plotEnvelope [envAsr 0.1 1 1 (EnvNum (-4))
             ,envAsr 0.3 0.25 1 EnvSin
             ,envAsr 0.01 0.5 1.25 EnvLin]

---- ; drawings ; an envelope with a long release time that mostly sustains and then decays quickly
plotEnvelope [envAsr_c 0.01 1 0.75 (EnvNum (-4),EnvNum 4)
             ,envAsr_c 0.15 1 1.25 (EnvNum (-4),EnvNum (-4))
             ,envAsr_c 0.15 1 1.25 (EnvNum (-4),EnvNum 64)]

