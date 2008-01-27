bufDelayC buf in delayTime

Buffer based simple delay line with cubic interpolation.

Simple delay line with cubic interpolation which uses a buffer for
its internal memory. See also BufDelayN which uses no
interpolation, and BufDelayL which uses linear interpolation. Cubic
interpolation is more computationally expensive than linear, but
more accurate.

See also DelayC.

buf       - buffer number.
in        - the input signal.
delaytime - delay time in seconds.

> withSC3 (\fd -> async fd (b_alloc 0 44100 1))

> do { d <- dust AR 1
>    ; n <- whiteNoise AR
>    ; let x = decay d 0.5 * n * 0.3
>      in audition (out 0 (bufDelayC 0 x 0.2 + x)) }
