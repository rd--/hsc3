> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.Common.Buffer.Gen as Gen {- hsc3 -}

function to generate wavetable buffer using b_gen_cheby

> mk_b a =
>     let tbl_f = [Normalise,Wavetable,Clear]
>         msg = [b_alloc 10 512 1,b_gen_cheby 10 tbl_f a]
>     in withSC3 (mapM_ async msg)

    > mk_b [1,0,1,1,0,1]

hear waveshaper at pure (sin) tone

> g_01 =
>     let z = sinOsc AR 300 0 * line KR 0 1 6 DoNothing
>     in shaper 10 z * 0.1

plot wavetable (as in-buffer layout, as plain wavetable)

    > import Sound.SC3.Plot {- hsc3-plot -}
    > withSC3 (plot_buffer1 10)
    > withSC3 (plot_wavetable1 10)

![](sw/hsc3/Help/SVG/shaper.0.svg)
![](sw/hsc3/Help/SVG/shaper.1.svg)

variations

    > mk_b [0.25,0.5,0.25]

> g_02 =
>     let z = sinOsc AR 400 (pi / 2) * line KR 0 1 6 DoNothing
>     in shaper 10 z * 0.1

wave shape external signal

    > mk_b [1,0,1,1,0,1]

> g_03 =
>     let z = soundIn 0
>         x = sinOsc KR (1/4) 0
>     in xFade2 z (shaper 10 z) x 0.5

    > mk_b [1,0,1,1,0,1,0.5,0,0.25,0,0.75,1]

> g_04 =
>     let z = soundIn 0
>         x = mouseX KR (-1) 1 Linear 0.2
>     in xFade2 z (shaper 10 z) x 0.5

generate table and use localBuf

> t_01 :: (Enum n,Floating n,Ord n) => [n]
> t_01 = Gen.cheby 257 [1,0,1,1,0,1] -- [1,0,1,1,0,1,0.5,0,0.25,0,0.75,1]

> t_02 :: (Enum n,Floating n,Ord n) => [n]
> t_02 = to_wavetable_nowrap t_01

    > plotTable1 t_01
    > plotTable1 t_02
    > length t_02 == 512

![](sw/hsc3/Help/SVG/shaper.2.svg)
![](sw/hsc3/Help/SVG/shaper.3.svg)

> g_05 =
>     let z = sinOsc AR 300 0 * line KR 0 1 6 DoNothing
>         b = asLocalBuf 'α' t_02
>     in shaper b z * 0.1
