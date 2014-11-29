> Sound.SC3.UGen.Help.viewSC3Help "Shaper"
> Sound.SC3.UGen.DB.ugenSummary "Shaper"

> import Sound.SC3

function to generate wavetable buffer using b_gen_cheby

> let mk_b a = do {_ <- async (b_alloc 10 512 1)
>                 ;let f = [Normalise,Wavetable,Clear]
>                  in async (b_gen_cheby 10 f a)}

hear waveshaper at pure (sin) tone

> let z = sinOsc AR 300 0 * line KR 0 1 6 RemoveSynth
> in withSC3 (do {_ <- mk_b [1,0,1,1,0,1]
>                ;play (out 0 (shaper 10 z * 0.1))})

plot wavetable (as in-buffer layout, as plain wavetable)

> import Sound.SC3.Plot {- hsc3-plot -}

> withSC3 (plot_buffer1 10)
> withSC3 (plot_wavetable1 10)

variations

> let z = sinOsc AR 400 (pi / 2) * line KR 0 1 6 RemoveSynth
> in withSC3 (do {_ <- mk_b [0.25,0.5,0.25]
>                ;play (out 0 (shaper 10 z * 0.1))})

wave shape external signal

> let {z = soundIn 0
>     ;x = sinOsc KR (1/4) 0}
> in withSC3 (do {_ <- mk_b [1,0,1,1,0,1]
>                ;play (out 0 (xFade2 z (shaper 10 z) x 0.5))})

> let {z = soundIn 4
>     ;x = mouseX KR (-1) 1 Linear 0.2}
> in withSC3 (do {_ <- mk_b [1,0,1,1,0,1,0.5,0,0.25,0,0.75,1]
>                ;play (out 0 (xFade2 z (shaper 10 z) x 0.5))})
