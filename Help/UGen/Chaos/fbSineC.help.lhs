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

> audition $ fbSineC AR (sampleRate / 4) 1 0.1 1.1 0.5 0.1 0.1 * 0.2

Increase feedback

> let fb = line KR 0.01 4 10 DoNothing
> audition $ fbSineC AR sampleRate 1 fb 1.1 0.5 0.1 0.1 * 0.2

Increase phase multiplier

> let a = line KR 1 2 10 DoNothing
> audition $ fbSineC AR sampleRate 1 0 a 0.5 0.1 0.1 * 0.2

Randomly modulate parameters

> let x = mouseX KR 1 12 Linear 0.1
> n0 <- return . (+ 1e4)  . (* 1e4)  =<< lfNoise2 KR x
> n1 <- return . (+ 33)   . (* 32)   =<< lfNoise2 KR x
> n2 <- return . (+ 0)    . (* 0.5)  =<< lfNoise2 KR x
> n3 <- return . (+ 1.05) . (* 0.05) =<< lfNoise2 KR x
> n4 <- return . (+ 0.3)  . (* 0.3)  =<< lfNoise2 KR x
> audition $ fbSineC AR n0 n1 n2 n3 n4 0.1 0.1 * 0.2
