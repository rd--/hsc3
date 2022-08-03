-- envAdsr ; random release time
let g = setResetFF 1 (dustId 'α' kr 1)
    p = envAdsr 0.01 0.25 0.15 0.75 1 (EnvNum (-4)) 0
    e = envGen kr g 0.1 0 1 RemoveSynth p
in sinOsc ar 440 0 * e

-- envAdsr
let c = control kr "gate" 1
    p = envAdsr 0.75 2.75 0.1 7.25 1 (EnvNum (-4)) 0
    e = envGen kr c 1 0 1 RemoveSynth p
in sinOsc ar 440 0 * e * 0.1

-- envAdsr ; record variant
let g = control kr "gate" 1
    c = EnvNum (-4)
    r = Adsr {adsr_attackTime = 0.75
             ,adsr_decayTime = 0.75
             ,adsr_sustainLevel = 0.5
             ,adsr_releaseTime = 0.75
             ,adsr_peakLevel = 1
             ,adsr_curve = (c,c,c)
             ,adsr_bias = 0}
    p = envAdsr_r r
    e = envGen kr g 0.1 0 1 DoNothing p
in sinOsc ar 440 0 * e

-- envAdsr ; adsr_def
let g = control kr "gate" 1
    p = envAdsr_r adsr_def
    e = envGen kr g 0.1 0 1 DoNothing p
in sinOsc ar 440 0 * e

---- ; close gate message
withSc3 (Sound.OSC.sendMessage (n_set1 (-1) "gate" 0))

---- ; drawings
import Sound.Sc3.Plot {- hsc3 -}
plotEnvelope [envAdsr 0.75 0.75 0.5 0.75 1 (EnvNum (-4)) 0
             ,envAdsr 0.02 0.2 0.25 1 1 (EnvNum (-4)) 0
             ,envAdsr 0.001 0.2 0.25 1 1 (EnvNum (-4)) 0
             ,envAdsr 0 2 1 0.1 0.5 EnvSin 0
             ,envAdsr 0.001 1.54 1 0.001 0.4 EnvSin 0]

---- ; help
Sound.Sc3.Lang.Help.viewSc3Help "Env.*adsr"

---- ; coord ; [0,3,2,-99,1,0.01,5,-4,0.5,0.3,5,-4,0,1,5,-4]
envelope_sc3_array (envAdsr 0.01 0.3 0.5 1 1 (EnvNum (-4)) 0)

---- ; coord ; [0,3,2,-99,1,0.3,5,-4,0.1,0.4,5,-4,0,1.2,5,-4]
envelope_sc3_array (envAdsr 0.3 0.4 0.1 1.2 1 (EnvNum (-4)) 0)

---- ; sc3
Env.adsr.asArray == [0,3,2,-99,1,0.01,5,-4,0.5,0.3,5,-4,0,1,5,-4];
x = {|gate=0, freq=440 | EnvGen.kr(Env.adsr,gate) * SinOsc.ar(freq,0) * 0.1}.play
x.set(\gate,1);
x.set(\gate,0);
