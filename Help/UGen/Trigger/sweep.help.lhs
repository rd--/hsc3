sweep trig rate

Triggered linear ramp.  Starts a linear raise by rate/sec from zero
when trig input crosses from non-positive to positive.
	
Using sweep to modulate sine frequency

> let { x = mouseX KR 0.5 20 Exponential 0.1
>     ; t = impulse KR x 0 
>     ; f = sweep t 700 + 500 }
> in audition (out 0 (sinOsc AR f 0 * 0.2))

Using sweep to index into a buffer

> withSC3 (\fd -> send fd (b_allocRead 0 "/home/rohan/audio/metal.wav" 0 0))

> let { x = mouseX KR 0.5 20 Exponential 0.1
>     ; t = impulse AR x 0
>     ; p = sweep t (bufSampleRate KR 0) }
> in audition (out 0 (bufRdL 1 AR 0 p NoLoop))

Backwards, variable offset

> do { n <- lfNoise0 KR 15
>    ; let { x = mouseX KR 0.5 10 Exponential 0.1
>          ; t = impulse AR x 0
>          ; r = bufSampleRate KR 0
>          ; p = sweep t (negate r) + (bufFrames KR 0 * n) }
>      in audition (out 0 (bufRdL 1 AR 0 p NoLoop)) }

Raising rate

> let { x = mouseX KR 0.5 10 Exponential 0.1
>     ; t = impulse AR x 0
>     ; r = sweep t 2 + 0.5
>     ; p = sweep t (bufSampleRate KR 0 * r) }
> in audition (out 0 (bufRdL 1 AR 0 p NoLoop))
