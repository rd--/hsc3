henonN freq a b x0 x1
henonL freq a b x0 x1
henonC freq a b x0 x1

Henon map chaotic generator.

freq   - iteration frequency in Hertz   -- 22050
a, b   - equation variables             -- 1.4, 0.3
x0, x1 - initial and second values of x -- 0, 0

A non-interpolating sound generator based on the difference
equation:

    xn + 2 = 1 - axn + 12 + bxn

This equation was discovered by French astronomer Michel Henon
while studying the orbits of stars in globular clusters.

With default initial parameters.

> let x = mouseX KR 20 sampleRate Linear 0.1
> audition $ henonN AR x 1.4 0.3 0 0 * 0.1

With mouse-control of parameters.

> let x = mouseX KR 1 1.4 Linear 0.1
>     y = mouseY KR 0 0.3 Linear 0.1
> audition $ henonN AR (sampleRate / 4) x y 0 0 * 0.1

With randomly modulate parameters.

> n0 <- return . (+ 1.20) . (* 0.20) =<< lfNoise2 KR 1
> n1 <- return . (+ 0.15) . (* 0.15) =<< lfNoise2 KR 1
> audition $ henonN AR (sampleRate / 8) n0 n1 0 0 * 0.1

As a frequency control.

> let x = mouseX KR 1 1.4 Linear 0.1
>     y = mouseY KR 0 0.3 Linear 0.1
>     f = 40
> audition $ sinOsc AR (henonN AR f x y 0 0 * 800 + 900) 0 * 0.4
