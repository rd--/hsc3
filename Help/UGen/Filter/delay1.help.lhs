delay1 in

Fixed Single sample delay.

> let s = impulse AR 1 0
> audition (out 0 (s + (delay1 s)))
