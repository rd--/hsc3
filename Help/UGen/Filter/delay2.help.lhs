delay2 in

Fixed two sample delay.

> let s = impulse AR 1 0
> audition (out 0 (s + (delay2 s)))
