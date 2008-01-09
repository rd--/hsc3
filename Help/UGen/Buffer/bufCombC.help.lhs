bufCombC buf in delayTime decayTime

Buffer based comb delay line with cubic interpolation

All pass delay line with cubic interpolation which uses a buffer
for its internal memory. See also BufCombN which uses no
interpolation, and BufCombL which uses linear interpolation. Cubic
interpolation is more computationally expensive than linear, but
more accurate.  See also CombC.

buf       - buffer number.
in        - the input signal.
delaytime - delay time in seconds.
decaytime - time for the echoes to decay by 60 decibels. If this
            time is negative then the feedback coefficient will be
            negative, thus emphasizing only odd harmonics at an
            octave lower.

> withSC3 (\fd -> do send fd (b_alloc 0 44100 1) >> wait fd "/done")

> do { d <- dust AR 1
>    ; n <- whiteNoise AR
>    ; let x = decay d 0.2 * n * 0.25
>      in audition (out 0 (bufCombC 0 x 0.25 6)) }
