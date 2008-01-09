done src

Outputs a unit signal if the 'done' flag of the unit at `src' is
set, else output zero.

> let { x = mouseX KR (-1) 1 Linear 0.1
>     ; e = linen x 0.1 0.1 0.5 DoNothing 
>     ; o1 = sinOsc AR 880 0 * 0.1
>     ; o2 = sinOsc AR 440 0 * e }
> in audition (out 0 (mce [ done e * o1, o2 ]))
