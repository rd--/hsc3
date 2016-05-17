    Sound.SC3.UGen.Help.viewSC3Help "AtsSynth"
    Sound.SC3.UGen.DB.ugenSummary "AtsSynth"

> import System.IO.Unsafe {- base -}
> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.Data.ATS {- hsc3-data -}

> ats_fn_0 = "/home/rohan/data/audio/pf-c5.4.ats"
> ats_fn_1 = "/home/rohan/cvs/tn/tn-56/ats/metal.ats"

    > ats_load_sc3 0 ats_fn
    > withSC3 (b_query1_unpack 0)

read file

> ats_hdr_0 = unsafePerformIO (fmap ats_header (ats_read ats_fn_0))
> ats_hdr_1 = unsafePerformIO (fmap ats_header (ats_read ats_fn_1))

show header

    > putStrLn $ ats_header_pp ats_hdr_0

simple re-synthesiser, ATS data is at buffer 0

> g_01 =
>     let x = mouseX KR 0.0 1.0 Linear 0.2
>         y = mouseY KR 0.75 1.25 Linear 0.2
>         np = constant (ats_n_partials ats_hdr_0)
>     in atsSynth 0 np 0 1 x y 0
