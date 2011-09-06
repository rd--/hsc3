> Sound.SC3.UGen.Help.viewSC3Help "DemandEnvGen"
> Sound.SC3.UGen.DB.ugenSummary "DemandEnvGen"

> import Sound.SC3.ID
> import qualified Sound.SC3.Monadic as M

Frequency ramp, exponential curve.
> let {l = dseq 'a' dinf (mce2 440 9600)
>     ;y = mouseY' KR 0.01 3 Exponential 0.1
>     ;f = demandEnvGen AR l y 2 0 1 1 1 0 1 DoNothing}
> in audition (out 0 (sinOsc AR f 0 * 0.1))

Frequency envelope with random times.
> do {l <- M.dseq dinf (mce [204, 400, 201, 502, 300, 200])
>    ;t <- M.drand dinf (mce [1.01, 0.2, 0.1, 2.0])
>    ;let {y = mouseY' KR 0.01 3 Exponential 0.1
>         ;f = demandEnvGen AR l (t * y) 7 0 1 1 1 0 1 DoNothing}
>     in audition (out 0 (sinOsc AR (f * mce2 1 1.01) 0 * 0.1))}

short sequence with doneAction, linear
> let {s = dseq 'a' 1 (mce [1300,500,800,300,400])
>     ;f = demandEnvGen KR s 2 1 0 1 1 1 0 1 RemoveSynth}
> in audition (out 0 (sinOsc AR (f * mce2 1 1.01) 0 * 0.1))
