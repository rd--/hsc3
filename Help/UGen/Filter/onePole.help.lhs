onePole in coef

A one pole filter.  Implements the formula: out(i) = ((1 -
abs(coef)) * in(i)) + (coef * out(i-1)).

in   - input signal to be processed
coef - feedback coefficient. Should be between -1 and +1

> do { n <- whiteNoise AR
>    ; audition (out 0 (onePole (n * 0.5) 0.95)) }

> do { n <- whiteNoise AR
>    ; audition (out 0 (onePole (n * 0.5) (-0.95))) }

> do { n <- whiteNoise AR
>    ; let c = line KR (-0.99) 0.99 10 RemoveSynth
>      in audition (out 0 (onePole (n * 0.5) c)) }
