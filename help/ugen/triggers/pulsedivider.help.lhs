pulsedivider trig div start

Outputs one impulse each time it receives a certain number of triggers
at its input.  A trigger happens when the signal changes from
non-positive to positive.

> import Hsc

> g = a + b * 0.4
>     where p = impulse AR 8 0
>           d = pulsedivider p (MCE [4,7]) 0
>           a = sinosc AR 1200 0 * decay2 p 0.005 0.1
>           b = sinosc AR 600  0 * decay2 d 0.005 0.5

> ex = play' sc g

> drw = draw' g

