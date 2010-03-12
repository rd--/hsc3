bufDur rate bufnum

Current duration of buffer (in seconds).

> import Sound.SC3

> let fn = "/home/rohan/audio/metal.wav"
> in withSC3 (\fd -> async fd (b_allocRead 0 fn 0 0))

> let { t = impulse AR (recip (bufDur KR 0)) 0
>     ; p = sweep t (bufSampleRate KR 0) }
> in audition (out 0 (bufRdL 1 AR 0 p NoLoop))
