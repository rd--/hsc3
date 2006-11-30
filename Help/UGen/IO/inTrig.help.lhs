inTrig numChannels rate bus

Generate a trigger anytime a bus is set.

Any time the bus is "touched" ie. has its value set (using "/c_set"
etc.), a single impulse trigger will be generated.  Its amplitude
is the value that the bus was set to.

Run an oscillator with the trigger at bus 10.

> let t = inTrig 1 KR 10
>     e = envGen KR t t 0 1 0 envPerc'
> audition $ sinOsc ar 440 0 * e

Set bus 10.

> withSC3 (\fd -> send fd (c_set 10 0.1))
