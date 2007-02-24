wrapIndex bufnum in

Index into a table with a signal.

The input signal value is truncated to an integer value and used as
an index into the table.  Out of range index values are wrapped
cyclically to the valid range.

bufnum - index of the buffer
in     - the input signal.

> withSC3 (\fd -> do send fd (b_alloc 0 6 1)
>                    wait fd "/done"
>                    send fd (b_setn 0 [(0, [200, 300, 400, 500, 600, 800])]))

> let x = mouseX KR 0 18 Linear 0.1
>     f = wrapIndex 0 x
> audition $ sinOsc AR f 0 * 0.5
