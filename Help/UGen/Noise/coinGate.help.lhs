coinGate prob in

When it receives a trigger, it tosses a coin, and either passes the
trigger or doesn't.

> g <- coinGate 0.2 (impulse KR 10 0)
> f <- tRand 300.0 400.0 g
> audition $ sinOsc AR f 0 * 0.1
