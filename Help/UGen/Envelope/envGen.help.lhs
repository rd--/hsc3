envGen rate gate levelScale levelBias timeScale doneAction envelope

A segment based envelope generator.  Note that the SC3 language
reorders the inputs to this UGen so that the envelope is the first
argument.

There are utilities for contructing the envelope argument.

The arguments for levelScale, levelBias, and timeScale are polled
when the EnvGen is triggered and remain constant for the duration
of the envelope.

envelope - an breakpoint set

gate - this triggers the envelope and holds it open while > 0. If
       the Env is fixed-length (e.g. Env.linen, Env.perc), the gate
       argument is used as a simple trigger. If it is an sustaining
       envelope (e.g. Env.adsr, Env.asr), the envelope is held open
       until the gate becomes 0, at which point is released.

levelScale - scales the levels of the breakpoints.

levelBias - offsets the levels of the breakpoints.

timeScale - scales the durations of the segments.

doneAction - an integer representing an action to be executed when
             the env is finished playing. This can be used to free
             the enclosing synth, etc.

Percussive envelope

> let p = envPerc 0.01 1 1 [EnvNum (-4), EnvNum (-4)]
>     e = envGen KR 1 0.1 0 1 RemoveSynth p
> audition (sinOsc AR 440 0 * e)

Sine envelope

> let s = envSine 9 0.1
>     e = envGen KR 1 1 0 1 RemoveSynth s
> audition (sinOsc AR 440 0 * e)

Co-ordinate (break-point) envelope

> let c = envCoord [(0,0), (0.5, 0.1), (0.55, 1), (1, 0)] 9 0.1 EnvLin
>     e = envGen KR 1 1 0 1 RemoveSynth c
> audition (sinOsc AR 440 0 * e)

Trapezoidal envelope

> let t = envTrapezoid 0.05 0.95 3 0.1
>     e = envGen KR 1 1 0 1 RemoveSynth t
> audition (sinOsc AR 440 0 * e)
