    Sound.SC3.UGen.Help.viewSC3Help "Env.*adsr"
    :i Sound.SC3.ADSR
    :t Sound.SC3.envADSR_r
    :t Sound.SC3.envADSR

> import Sound.SC3 {- hsc3 -}

{SinOsc.ar * EnvGen.kr(Env.adsr(0.75, 2.75, 0.1, 7.25, 1, -4, 0))}.draw;

> g_01 =
>     let c = control KR "env-gate" 1
>         p = envADSR 0.75 2.75 0.1 7.25 1 (EnvNum (-4)) 0
>         e = envGen KR c 1 0 1 DoNothing p
>     in sinOsc AR 440 0 * e * 0.1

    import Sound.OSC {- hosc -}
    withSC3 (sendMessage (n_set1 (-1) "env-gate" 0))
    withSC3 (sendMessage (n_set1 (-1) "env-gate" 1))
    withSC3 (sendMessage (n_free [-1]))

> e_01 =
>     [envADSR 0.75 0.75 0.5 0.75 1 (EnvNum (-4)) 0
>     ,envADSR 0.02 0.2 0.25 1 1 (EnvNum (-4)) 0
>     ,envADSR 0.001 0.2 0.25 1 1 (EnvNum (-4)) 0
>     ,envADSR 0 2 1 0.1 0.5 EnvSin 0
>     ,envADSR 0.001 1.54 1 0.001 0.4 EnvSin 0]

    import Sound.SC3.Plot {- hsc3 -}
    plotEnvelope e_01

There is a record variant:

> g_02 =
>     let g = control KR "gate" 1
>         c = EnvNum (-4)
>         r = ADSR {adsr_attackTime = 0.75
>                  ,adsr_decayTime = 0.75
>                  ,adsr_sustainLevel = 0.5
>                  ,adsr_releaseTime = 0.75
>                  ,adsr_peakLevel = 1
>                  ,adsr_curve = (c,c,c)
>                  ,adsr_bias = 0}
>         p = envADSR_r r
>         e = envGen KR g 0.1 0 1 DoNothing p
>     in sinOsc AR 440 0 * e

    withSC3 (sendMessage (n_set1 (-1) "gate" 0))
    withSC3 (sendMessage (n_set1 (-1) "gate" 1))
    withSC3 (sendMessage (n_free [-1]))

SC3 comparison:

Env.adsr.asArray == [0,3,2,-99,1,0.01,5,-4,0.5,0.3,5,-4,0,1,5,-4];

> r_03 = [0,3,2,-99,1,0.01,5,-4,0.5,0.3,5,-4,0,1,5,-4]
> e_03 = envADSR 0.01 0.3 0.5 1 1 (EnvNum (-4)) 0
> a_03 = envelope_sc3_array e_03

    e_03 == envADSR_r adsrDefault
    a_03 == Just r_03

> r_04 = [0,3,2,-99,1,0.3,5,-4,0.1,0.4,5,-4,0,1.2,5,-4]
> e_04 = envADSR 0.3 0.4 0.1 1.2 1 (EnvNum (-4)) 0
> a_04 = envelope_sc3_array e_04

    a_04 == Just r_04

x = {|gate=0, freq=440 | EnvGen.kr(Env.adsr,gate) * SinOsc.ar(freq,0) * 0.1}.play
x.set(\gate,1);
x.set(\gate,0);
