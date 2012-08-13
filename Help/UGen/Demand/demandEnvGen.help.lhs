> Sound.SC3.UGen.Help.viewSC3Help "DemandEnvGen"
> Sound.SC3.UGen.DB.ugenSummary "DemandEnvGen"

> import Sound.SC3.ID

Frequency ramp, exponential curve.
> let {l = dseq 'a' dinf (mce2 440 9600)
>     ;y = mouseY KR 0.01 3 Exponential 0.2
>     ;s = env_curve_shape EnvExp
>     ;f = demandEnvGen AR l y s 0 1 1 1 0 1 DoNothing}
> in audition (out 0 (sinOsc AR f 0 * 0.1))

Frequency envelope with random times.
> let {l = dseq 'a' dinf (mce [204,400,201,502,300,200])
>     ;t = drand 'a' dinf (mce [1.01,0.2,0.1,2.0])
>     ;y = mouseY KR 0.01 3 Exponential 0.2
>     ;s = env_curve_shape EnvCub
>     ;f = demandEnvGen AR l (t * y) s 0 1 1 1 0 1 DoNothing}
> in audition (out 0 (sinOsc AR (f * mce2 1 1.01) 0 * 0.1))

frequency modulation
> let {n = dwhite 'a' dinf 200 1000
>     ;x = mouseX KR (-0.01) (-4) Linear 0.2
>     ;y = mouseY KR 1 3000 Exponential 0.2
>     ;s = env_curve_shape (EnvNum undefined)
>     ;f = demandEnvGen AR n (sampleDur * y) s x 1 1 1 0 1 DoNothing
>     ;o = sinOsc AR f 0 * 0.1}
> in audition (out 0 o)

short sequence with doneAction, linear
> let {l = dseq 'a' 1 (mce [1300,500,800,300,400])
>     ;s = env_curve_shape EnvLin
>     ;f = demandEnvGen KR l 2 s 0 1 1 1 0 1 RemoveSynth}
> in audition (out 0 (sinOsc AR (f * mce2 1 1.01) 0 * 0.1))

gate, mouse x on right side of screen toggles gate
> let {n = roundTo (dwhite 'a' dinf 300 1000) 100
>     ;x = mouseX KR 0 1 Linear 0.2
>     ;g = x >* 0.5
>     ;f = demandEnvGen AR n 0.1 5 0.3 g 1 1 0 1 DoNothing
>     ;o = sinOsc AR (f * mce2 1 1.21) 0 * 0.1}
> in audition (out 0 o)

gate
mouse x on right side of screen toggles sample and hold
mouse button does hard reset
> let {l = dseq 'a' 2 (mce [dseries 'a' 5 400 200,500,800,530,4000,900])
>     ;x = mouseX KR 0 1 Linear 0.2
>     ;g = (x >* 0.5) - 0.1
>     ;b = mouseButton KR 0 1 0.2
>     ;r = (b >* 0.5) * 2
>     ;s = env_curve_shape EnvSin
>     ;f = demandEnvGen KR l 0.1 s 0 g r 1 0 1 DoNothing
>     ;o = sinOsc AR (f * mce2 1 1.001) 0 * 0.1}
> in audition (out 0 o)

initialise coordinate buffer
layout is (initial-level,duration,level,..,loop-duration)
> withSC3 (async (b_alloc_setn1 0 0 [0,0.5,0.1,0.5,1,0.01]))

> let {l_i = dseries 'a' dinf 0 2
>     ;d_i = dseries 'b' dinf 1 2
>     ;l = dbufrd 'a' 0 l_i Loop
>     ;d = dbufrd 'b' 0 d_i Loop
>     ;s = env_curve_shape EnvLin
>     ;e = demandEnvGen KR l d s 0 1 1 1 0 5 RemoveSynth
>     ;f = midiCPS (60 + (e * 12))}
> in audition (out 0 (sinOsc AR (f * mce2 1 1.01) 0 * 0.1))
