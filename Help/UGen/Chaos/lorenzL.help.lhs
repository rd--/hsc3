lorenzL rate freq s r b h xi yi zi

freq    - iteration frequency in Hertz
s, r, b - equation variables
h       - integration time step
xi      - initial value of x
yi      - initial value of y
zi      - initial value of z

Lorenz chaotic generator.  A strange attractor discovered by Edward
N. Lorenz while studying mathematical models of the atmosphere.
The system is composed of three ordinary differential equations:

x' = s(y - x)
y' = x(r - z) - y
z' = xy - bz

The time step amount h determines the rate at which the ODE is
evaluated.  Higher values will increase the rate, but cause more
instability.  A safe choice is the default amount of 0.05.

Vary frequency

> import Sound.SC3

> let x = mouseX KR 20 sampleRate Linear 0.1
> in audition (out 0 (lorenzL AR x 10 27 2.667 0.05 0.1 0 0 * 0.3))

Randomly modulate params

> import Sound.SC3.Monadic

> let { madd a m = return . (+ a) . (* m)
>     ; n = lfNoise0 KR 1 }
> in do { n0 <- madd 10 2 =<< n
>       ; n1 <- madd 38 20 =<< n
>       ; n2 <- madd 2 1.5 =<< n
>       ; let o = lorenzL AR sampleRate n0 n1 n2 0.05 0.1 0 0 * 0.2
>         in audition (out 0 o) }

As frequency control

> let { x = mouseX KR 1 200 Linear 0.1
>     ; n = lorenzL AR x 10 28 2.667 0.05 0.1 0 0 }
> in audition (out 0 (sinOsc AR (lag n 0.003 * 800 + 900) 0 * 0.4))
