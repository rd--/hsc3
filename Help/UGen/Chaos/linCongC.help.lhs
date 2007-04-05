linCongC rate freq a c m xi
linCongL rate freq a c m xi
linCongN rate freq a c m xi

Linear congruential chaotic generator.

freq - iteration frequency in Hertz
a    - multiplier amount
c    - increment amount
m    - modulus amount
xi   - initial value of x

A cubic-interpolating sound generator based on the difference
equation:

	xn+1 = (axn + c) % m

The output signal is automatically scaled to a range of [-1, 1].


Default initial parameters.

> let x = mouseX KR 20 sampleRate Linear 0.1
> audition (out 0 (linCongC AR x 1.1 0.13 1 0 * 0.2))

Randomly modulate parameters.

> [n0, n1, n2, m] <- mapM (lfNoise2 KR) [1.0, 0.1, 0.1, 0.1]
> let f = n0 * 1e4 + 1e4
>     a = n1 * 0.5 + 1.4
>     c = n2 * 0.1 + 0.1
> audition (out 0 (linCongC AR f a c m 0 * 0.2))
