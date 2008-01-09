timer trig

Returns time since last triggered
	
Using timer to modulate sine frequency: the slower the trigger is
the higher the frequency

> let t = impulse KR (mouseX KR 0.5 20 Exponential 0.1) 0
> in audition (out 0 (sinOsc AR (timer t * 500 + 500) 0 * 0.2))
