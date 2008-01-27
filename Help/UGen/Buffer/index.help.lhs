index bufnum in

Index into a table with a signal.  The input signal value is
truncated to an integer value and used as an index into the table.
Out of range index values are clipped to the valid range.

Allocate and set values at buffer 10.

> withSC3 (\fd -> do { async fd (b_alloc 10 6 1)
>                    ; send fd (b_setn 10 [(0, [50, 100, 200, 400, 800, 1600])]) })

Index into the above buffer for frequency values.

> let f = index 10 (lfSaw KR 2 3 * 4)
> in audition (out 0 (sinOsc AR (mce [f, f * 9]) 0 * 0.1))

Free buffer.

> withSC3 (\fd -> send fd (b_free 10))
