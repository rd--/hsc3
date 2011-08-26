gVerb in roomsize=10 revtime=3 damping=0.5 inputbw=0.5 spread=15
      drylevel=1 earlyreflevel=0.7 taillevel=0.5 maxroomsize=300

A less-simple reverb.

> import Sound.SC3

> let { i = impulse AR 1 0
>     ; c = lfCub AR 1200 0
>     ; s = decay i 0.25 * c * 0.1
>     ; r = gVerb s 10 3 0.5 0.5 15 1 0.7 0.5 300 }
> in audition (out 0 r)
