inTrig numChannels bus

Generate a trigger anytime a bus is set.

Any time the bus is "touched" ie. has its value set (using "/c_set"
etc.), a single impulse trigger will be generated.  Its amplitude
is the value that the bus was set to.

Run an oscillator with the trigger at bus 10.

> let { t = inTrig 1 10
>     ; e = envGen KR t t 0 1 DoNothing envPerc' }
> in audition (out 0 (sinOsc AR 440 0 * e))

Set bus 10, each set will trigger a ping.

> let c_set1 i n = c_set [(i,n)]
> in withSC3 (\fd -> send fd (c_set1 10 0.1))
