twindex in normalize array

Triggered windex.  When triggered, returns a random index value based
on array as a list of probabilities.  By default the list of
probabilities should sum to 1, when the normalize flag is set to 1,
the values get normalized by the ugen (less efficient) Assuming
normalized values

> import Sound.SC3.Monadic

> let { p = mce [1/5, 2/5, 2/5]
>     ; a = mce [400, 500, 600]
>     ; t = impulse KR 6 0 }
> in do { i <- twindex t 0 p
>       ; audition (out 0 (sinOsc AR (select i  a) 0 * 0.1)) }

Modulating probability values

> let { p = mce [1/4, 1/2, sinOsc KR 0.3 0 * 0.5 + 0.5]
>     ; a = mce [400, 500, 600]
>     ; t = impulse KR 6 0 }
> in do { i <- twindex t 1 p
>       ; audition (out 0 (sinOsc AR (select i a) 0 * 0.1)) }
