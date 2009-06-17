cuspN rate freq a b xi
cuspL rate freq a b xi

freq - iteration frequency in Hertz
a, b - equation variables
xi   - initial value of x

Cusp map chaotic generator.  Non- and linear- interpolating sound
generator based on the difference equation:

xn+1 = a - b*sqrt(|xn|)

Vary frequency

> import Sound.SC3

> let x = mouseX KR 20 sampleRate Linear 0.1
> in audition (out 0 (cuspL AR x 1.0 1.99 0 * 0.3))

Mouse-controlled parameters.

> let { x = mouseX KR 0.9 1.1 Linear 0.1
>     ; y = mouseY KR 1.8 2.0 Linear 0.1 }
> in audition (out 0 (cuspL AR (sampleRate / 4) x y 0 * 0.3))

As frequency control.

> let { x = mouseX KR 0.9 1.1 Linear 0.1
>     ; y = mouseY KR 1.8 2.0 Linear 0.1
>     ; n = cuspL AR 40 x y 0 * 0.3 }
> in audition (out 0 (sinOsc AR (n * 800 + 900) 0 * 0.4))
