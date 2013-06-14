> Sound.SC3.UGen.Help.viewSC3Help "PV_BinDelay"
> Sound.SC3.UGen.DB.ugenSummary "PV_BinDelay"

> import Sound.SC3

function to allocate buffers (fft,delay,feedback)
> let mk_b sz = withSC3 (do {_ <- async (b_alloc 10 (sz * 2) 1)
>                           ;_ <- async (b_alloc 11 sz 1)
>                           ;async (b_alloc 12 sz 1)})

allocate buffers (number of bins)
> mk_b 128

function to generate bindelay filter
> let mk_u z = let {maxdel = 0.5
>                  ;c1 = fft 10 z 0.25 0 1 0
>                  ;c2 = pv_BinDelay c1 maxdel 11 12 0.25}
>              in z + ifft c2 0 0

start filter
> audition (out 0 (mk_u (soundIn 4)))

set delay times (unary)
> withSC3 (send (b_fill 11 [(0,128,0.25)]))

set feedback gain
> withSC3 (send (b_fill 12 [(0,128,0.75)]))

function to generate sin table of n places in range (l,r)
> let gen_sin l r n ph =
>     let f x = range l r (sin ((x / n) * 2 * pi + ph))
>     in map f [0..n]

set delay times (sin)
> withSC3 (send (b_setn1 11 0 (gen_sin 0 0.35 128 0)))

set feedback gain (sin)
> withSC3 (send (b_setn1 12 0 (gen_sin 0.75 0.95 128 pi)))

modulate delay times (lfo)
> let o = range 0.15 0.35 (blip KR (1/23) 3)
> in audition (recordBuf KR 11 0 1 0 1 Loop 1 DoNothing o)

modulate feedback gains (lfo)
> let o = range 0.75 0.95 (blip KR (1/25) 5)
> in audition (recordBuf KR 12 0 1 0 1 Loop 1 DoNothing o)
