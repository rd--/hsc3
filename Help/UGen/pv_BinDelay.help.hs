
> gr_02 =
>     let o = range 0.15 0.35 (blip KR (1/23) 3)
>     in recordBuf KR 11 0 1 0 1 Loop 1 DoNothing o

modulate feedback gains (lfo)

> gr_03 =
>     let o = range 0.75 0.95 (blip KR (1/25) 5)
>     in recordBuf KR 12 0 1 0 1 Loop 1 DoNothing o
