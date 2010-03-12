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

> import Sound.SC3.Monadic

> do { r <- dust KR 1
>    ; s <- dgeom dinf (midiCPS 72) (midiRatio 1)
>    ; let { t = impulse KR 10 0
>          ; f = demand t r s 
>          ; o = sinOsc AR (mce [f, f + 0.7]) 0 }
>      in audition (out 0 (max (cubed o) 0 * 0.1)) }

> do { n <- diwhite dinf 60 72
>    ; let { t = impulse KR 10 0
>          ; s = midiCPS n
>          ; f = demand t 0 s
>          ; o = sinOsc AR (mce [f, f + 0.7]) 0 }
>      in audition (out 0 (cubed (cubed o) * 0.1)) }
