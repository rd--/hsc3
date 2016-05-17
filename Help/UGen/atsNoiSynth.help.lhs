    Sound.SC3.UGen.Help.viewSC3Help "AtsNoiSynth"
    Sound.SC3.UGen.DB.ugenSummary "AtsNoiSynth"

> import System.IO.Unsafe {- base -}
> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.Data.ATS {- hsc3-data -}
>
> ats_fn_0 = "/home/rohan/data/audio/pf-c5.4.ats"
> ats_fn_1 = "/home/rohan/cvs/tn/tn-56/ats/metal.ats"

load ATS file at scsynth

    > ats_load_sc3 0 ats_fn_0
    > withSC3 (b_query1_unpack 0)

read ATS header

> ats_hdr_0 = unsafePerformIO (fmap ats_header (ats_read ats_fn_0))
> ats_hdr_1 = unsafePerformIO (fmap ats_header (ats_read ats_fn_1))

    > putStrLn $ ats_header_pp ats_hdr_0
    > putStrLn $ ats_header_pp ats_hdr_1

run re-synthesis

> g_01 =
>     let np = constant (ats_n_partials ats_hdr_0)
>         ptr = lfSaw KR (constant (1 / ats_analysis_duration ats_hdr_0)) 1 * 0.5 + 0.5
>     in atsNoiSynth 0 np 0 1 ptr 1 0.1 1 0 25 0 1

> g_02 =
>     let x = mouseX KR 0.0 1.0 Linear 0.2
>         y = mouseY KR 0.0 1.0 Linear 0.2
>         np = constant (ats_n_partials ats_hdr_0)
>     in atsNoiSynth 0 np 0 1 x (1 - y) y 1 0 25 0 1

