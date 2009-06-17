fbSineC rate freq im fb a c xi yi
fbSineL rate freq im fb a c xi yi
fbSineN rate freq im fb a c xi yi

Feedback sine with chaotic phase indexing.

freq - iteration frequency in Hz    - 22050
im   - index multiplier amount      - 1
fb   - feedback amount              - 0.1
a    - phase multiplier amount      - 1.1
c    - phase increment amount       - 0.5
xi   - initial value of x           - 0.1
yi   - initial value of y           - 0.1

A cubic-interpolating sound generator based on the difference
equations:
	
	xn+1 = sin(im*yn + fb*xn)
	yn+1 = (ayn + c) % 2pi

This uses a linear congruential function to drive the phase
indexing of a sine wave.  For im = 1, fb = 0, and a = 1 a normal
sinewave results.

sclang default values

> import Sound.SC3

> let o = fbSineC AR (sampleRate / 4) 1 0.1 1.1 0.5 0.1 0.1 * 0.2
> in audition (out 0 o)

Increase feedback

> let { fb = line KR 0.01 4 10 DoNothing
>     ; o = fbSineC AR sampleRate 1 fb 1.1 0.5 0.1 0.1 * 0.2 }
> in audition (out 0 o)

Increase phase multiplier

> let { a = line KR 1 2 10 DoNothing
>     ; o = fbSineC AR sampleRate 1 0 a 0.5 0.1 0.1 * 0.2 }
> in audition (out 0 o)

Randomly modulate parameters

> let { madd a m = return . (+ a) . (* m)
>     ; x = mouseX KR 1 12 Linear 0.1 
>     ; n = lfNoise2 KR x }
> in do { n0 <- madd 1e4 1e4 =<< n
>       ; n1 <- madd 33 32 =<< n
>       ; n2 <- madd 0 0.5 =<< n
>       ; n3 <- madd 1.05 0.05 =<< n
>       ; n4 <- madd 0.3 0.3 =<< n
>       ; audition (out 0 (fbSineC AR n0 n1 n2 n3 n4 0.1 0.1 * 0.2)) }
