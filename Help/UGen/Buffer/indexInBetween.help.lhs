indexInBetween bufnum in

Interpolating index search into a sorted table with a signal.

Allocate and set values at buffer 10.

> withSC3 (\fd -> do send fd (b_alloc 10 6 1)
>                    wait fd "/done"
>                    send fd (b_setn 10 [(0, [200, 210, 400, 430, 600, 800])]))

Index into the above buffer for frequency values.

> let f0 = mouseX KR 200 900 Linear 0.1
>     i = indexInBetween 10 f0
>     l0 = index 10 i
>     l1 = index 10 (i + 1)
>     f1 = linLin (frac i) 0 1 l0 l1
> audition (out 0 (sinOsc AR (mce [f0, f1]) 0 * 0.1))

Free buffer.

> withSC3 (\fd -> send fd (b_free 10))
