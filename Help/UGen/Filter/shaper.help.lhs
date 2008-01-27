shaper bufnum in

Wave shaper.  Performs waveshaping on the input signal by indexing
into the table.

bufnum - the number of a buffer filled in wavetable format
         containing the transfer function.

in     - the input signal.

> let s = sinOsc AR 300 0 * line KR 0 1 6 RemoveSynth
> in withSC3 (\fd -> do { async fd (b_alloc 10 512 1)
>                       ; async fd (b_gen 10 "cheby" [0, 1, 0, 1, 1, 0, 1])
>                       ; audition (out 0 (shaper 10 s * 0.5)) })
