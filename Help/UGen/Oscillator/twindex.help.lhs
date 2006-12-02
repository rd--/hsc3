twindex in normalize array

Triggered windex.  When triggered, returns a random index value based
on array as a list of probabilities.  By default the list of
probabilities should sum to 1.0, when the normalize flag is set to 1,
the values get normalized by the ugen (less efficient) Assuming
normalized values

> let p = MCE [1/5, 2/5, 2/5]
>     a = MCE [400, 500, 600]
>     t = impulse KR 6 0
>     f = select (twindex t 0.0 p) a
> audition $ sinOsc AR f 0 * 0.2

Modulating probability values

> let p = MCE [1/4, 1/2, sinOsc KR 0.3 0 * 0.5 + 0.5]
>     a = MCE [400, 500, 600]
>     t = impulse KR 6 0
>     f = select (twindex t 1.0 p) a
> audition $ sinOsc AR f 0 * 0.2
