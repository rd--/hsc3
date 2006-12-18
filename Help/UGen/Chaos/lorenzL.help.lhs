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

> let x = mouseX KR 20 sampleRate Linear 0.1
> audition $ lorenzL AR x 10 27 2.667 0.05 0.1 0 0 * 0.3

Randomly modulate params

> n0 <- return . (+ 10) . (* 2)   =<< lfNoise0 KR 1
> n1 <- return . (+ 38) . (* 20)  =<< lfNoise0 KR 1
> n2 <- return . (+ 2)  . (* 1.5) =<< lfNoise0 KR 1
> audition $ lorenzL AR sampleRate n0 n1 n2 0.05 0.1 0 0 * 0.2

As frequency control

> let x = mouseX KR 1 200 Linear 0.1
>     n = lorenzL AR x 10 28 2.667 0.05 0.1 0 0 
> audition $ sinOsc AR (lag n 0.003 * 800 + 900) 0 * 0.4
