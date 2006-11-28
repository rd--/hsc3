done src

Outputs a unit signal if the 'done' flag of the unit at `src' is
set, else output zero.

> let x   = mouseX KR (-1) 1 Linear 0.1
>     env = linen x 0.1 0.1 0.5 0
> audition $ MCE [done env * sinOsc AR 880 0 * 0.1,
>                 sinOsc AR 440 0 * env]
