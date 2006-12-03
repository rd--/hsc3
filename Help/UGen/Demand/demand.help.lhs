demand trig reset ugens

Demand results from demand rate ugens.

When there is a trigger at the trig input, a value is demanded from
each ugen in the list and output. The unit generators in the list
should be 'demand' rate.

When there is a trigger at the reset input, the demand rate ugens
in the list are reset.

trig  - Trigger can be any signal. A trigger happens when
        the signal changes from non-positive to positive.

reset - Resets the list of ugens when triggered.

> r <- dust KR 1
> s <- dgeom 64 (midiCPS 72) (midiRatio 1)
> let t = impulse KR 10 0
>     f = demand t r s
> audition $ max (cubed (sinOsc AR (MCE [f, f + 0.7]) 0)) 0 * 0.1

> n <- diwhite 8192 60 72
> let t = impulse KR 10 0
>     s = midiCPS n
>     f = demand t 0 s
> audition $ cubed (cubed (sinOsc AR (MCE [f, f + 0.7]) 0)) * 0.1
