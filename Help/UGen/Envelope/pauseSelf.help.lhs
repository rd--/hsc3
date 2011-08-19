pauseSelf src

Pause enclosing synth when input signal crosses from non-positive to
positive.

> import Sound.SC3

> let { x = mouseX' KR (-1) 1 Linear 0.1
>     ; o = sinOsc AR 440 0 * 0.1 }
> in audition (mrg [pauseSelf x, out 0 o])

Run paused node (assuming no intermediate node is created).

> withSC3 (\fd -> send fd (n_run [(-1, True)]))
