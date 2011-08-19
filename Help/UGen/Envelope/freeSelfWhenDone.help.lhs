freeSelfWhenDone src

Free the synth when the 'done' flag of the unit at `src' is set.

> import Sound.SC3

> let { x = mouseX' KR (-1) 1 Linear 0.1
>     ; e = linen x 1 0.1 1 RemoveSynth }
> in audition (out 0 (sinOsc AR 440 0 * e))

> let { x = mouseX' KR (-1) 1 Linear 0.1
>     ; e = linen x 1 0.1 1 DoNothing }
> in audition (mrg [freeSelfWhenDone e, out 0 (sinOsc AR 440 0 * e)])
