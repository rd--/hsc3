    > Sound.SC3.UGen.Help.viewSC3Help "COsc"
    > Sound.SC3.UGen.DB.ugenSummary "COsc"

> import Sound.SC3 {- hsc3 -}

Allocate and fill buffer.

> gen_tbl b p = let f = [Normalise,Wavetable,Clear] in b_gen_sine1 b f p

> b_01 :: Num n => n {- or {-# Language NoMonomorphismRestriction #-} -}
> b_01 = 10
> b_01_setup = [b_alloc b_01 512 1,gen_tbl b_01 [1,1/2,1/3,1/4,1/5,1/6,1/7,1/8,1/9,1/10]]

    > withSC3 (mapM_ async b_01_setup)

Fixed beat frequency

> g_01 = cOsc AR b_01 200 0.7 * 0.1

Modulate beat frequency with mouseX

> g_02 = cOsc AR b_01 200 (mouseX KR 0 4 Linear 0.2) * 0.1

Compare with plain osc

> g_03 = osc AR b_01 200 0.0 * 0.1

Summing behaviour (<http://article.gmane.org/gmane.comp.audio.supercollider.devel/62575>)

> b_02 :: Num n => n
> b_02 = 11
> b_02_setup = [b_alloc b_02 512 1,gen_tbl b_02 [1]]

    > withSC3 (mapM_ async b_02_setup)
    > import Sound.SC3.Plot {- hsc3-plot -}
    > plot_ugen1 0.1 (cOsc AR 11 100 5)

![](sw/hsc3/Help/SVG/cOsc.0.svg)
