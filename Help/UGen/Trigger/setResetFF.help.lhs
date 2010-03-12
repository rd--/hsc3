setResetFF trig reset

Set-reset flip flop.  Output is set to 1.0 upon receiving a trigger
in the set input, and to 0.0 upon receiving a trigger in the reset
input. Once the flip flop is set to zero or one further triggers in
the same input are have no effect. One use of this is to have some
precipitating event cause something to happen until you reset it.

trig  - trigger sets output to one
reset - trigger resets output to zero

> import Sound.SC3.ID

> let { n = brownNoise 'α' AR
>     ; d0 = dust 'α' AR 5
>     ; d1 = dust 'β' AR 5 }
> in audition (out 0 (setResetFF d0 d1 * n * 0.2))
