    Sound.SC3.UGen.Help.viewSC3Help "AtsNoiSynth"
    Sound.SC3.UGen.DB.ugenSummary "AtsNoiSynth"

> import System.IO.Unsafe {- base -}
> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> import qualified Sound.SC3.Data.ATS as ATS {- hsc3-data -}

> ats_fn_0 = "/home/rohan/sw/hsc3-data/data/ats/pf-c5.4.ats"
> ats_fn_1 = "/home/rohan/sw/hsc3-data/data/ats/metal.ats"

load ATS file at scsynth

    > ATS.ats_load_sc3 0 ats_fn_0 -- ats_fn_1
    > withSC3 (b_query1_unpack 0)

read ATS header

> ats_hdr_0 = unsafePerformIO (fmap ATS.ats_header (ATS.ats_read ats_fn_0))
> ats_hdr_1 = unsafePerformIO (fmap ATS.ats_header (ATS.ats_read ats_fn_1))

    > putStrLn $ ATS.ats_header_pp ats_hdr_0
    > putStrLn $ ATS.ats_header_pp ats_hdr_1

run re-synthesis, sine & noise levels are at X and Y mouse controls

> g_01 =
>     let numPartials = constant (ATS.ats_n_partials ats_hdr_0)
>         dur = constant (ATS.ats_analysis_duration ats_hdr_0)
>         filePointer = lfSaw KR (1 / dur) 1 * 0.5 + 0.5
>         sinePct = mouseX KR 0 1 Linear 0.2
>         noisePct = mouseY KR 0 1 Linear 0.2
>     in X.atsNoiSynth AR 0 numPartials 0 1 filePointer sinePct noisePct 1 0 25 0 1

run re-synthesis, filePointer is at X-axis, sine & noise levels are both at Y

> f_02 numBands bandStart bandSkip =
>     let numPartials = constant (ATS.ats_n_partials ats_hdr_0)
>         filePointer = mouseX KR 0.0 1.0 Linear 0.2
>         noisePct = mouseY KR 0.0 1.0 Linear 0.2
>     in X.atsNoiSynth AR 0 numPartials 0 1 filePointer (1 - noisePct) noisePct 1 0 25 0 1

> g_02 = f_02 25 0 1
> g_03 = f_02 25 1 2
> g_04 = f_02 12 12 1
