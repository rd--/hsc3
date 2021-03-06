    Sound.SC3.Lang.Help.viewServerHelp "/b_gen"

function to generate wavetable buffer using b_gen_cheby

> mk_b a =
>     let tbl_f = [Normalise,Wavetable,Clear]
>         msg = [b_alloc 10 512 1,b_gen_cheby 10 tbl_f a]
>     in withSC3 (mapM_ async msg)

    > mk_b [1,0,1,1,0,1]
    > mk_b [0.25,0.5,0.25]
    > mk_b [1,0,1,1,0,1]
    > mk_b [1,0,1,1,0,1,0.5,0,0.25,0,0.75,1]

plot wavetable (as in-buffer layout, as plain wavetable)

    > import Sound.SC3.Plot {- hsc3-plot -}
    > withSC3 (plot_buffer1 10)
    > withSC3 (plot_wavetable1 10)
