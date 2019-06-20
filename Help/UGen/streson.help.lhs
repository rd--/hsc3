<http://www.csounds.com/manual/html/streson.html>

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> k_01 = recip (linExp (lfCub KR 0.1 (0.5 * pi)) (-1) 1 280 377)

    import Sound.SC3.Plot {- hsc3-plot -}
    plot_ugen_nrt (20,1) 16.0 k_01

> f_01 z = X.streson z k_01 0.9 * 0.3

> g_01 = f_01 (lfSaw AR (mce2 220 180) 0 * 0.2)

> g_02 = f_01 (soundIn 0)

see also Sound.SC3.Data.Modal.modal_frequency_ratios
