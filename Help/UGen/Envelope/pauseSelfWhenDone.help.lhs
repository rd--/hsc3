pauseSelfWhenDone src

Pauses the synth when the 'done' flag of the unit at `src' is set.

> let { x = mouseX KR (-1) 1 Linear 0.1
>     ; e = linen x 1 0.1 1 PauseSynth }
> in audition (out 0 (sinOsc AR 440 0 * e))

> let { x = mouseX KR (-1) 1 Linear 0.1
>     ; e = linen x 1 0.1 1 DoNothing 
>     ; o = sinOsc AR 440 0 * e }
> in audition (mrg [pauseSelfWhenDone e, out 0 o])
