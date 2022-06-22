> import System.IO.Unsafe {- base -}
> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}
> import qualified Sound.SC3.Data.ATS as ATS {- hsc3-data -}

> ats_fn_0 = "/home/rohan/sw/hsc3-data/data/ats/pf-c5.4.ats"
> ats_fn_1 = "/home/rohan/sw/hsc3-data/data/ats/metal.ats"

load ATS file at scsynth

    > ATS.ats_load_sc3 0 ats_fn_0
    > withSC3 (b_query1_unpack 0)

read ATS header

> ats_hdr_0 = unsafePerformIO (fmap ATS.ats_header (ATS.ats_read ats_fn_0))
> ats_hdr_1 = unsafePerformIO (fmap ATS.ats_header (ATS.ats_read ats_fn_1))

    > putStrLn $ ATS.ats_header_pp ats_hdr_0
    > putStrLn $ ATS.ats_header_pp ats_hdr_1

simple re-synthesiser, ATS data is at buffer 0, X is file pointer, Y is freq multiplier

> g_00 =
>   let numPartials = constant (ATS.ats_n_partials ats_hdr_0)
>       filePointer = mouseX kr 0.0 1.0 Linear 0.2
>       freqMul = mouseY kr 0.75 1.25 Linear 0.2
>   in X.atsSynth ar 0 numPartials 0 1 filePointer freqMul 0

> f_01 numPartials partialStart partialSkip freqMul freqAdd =
>     let dur = constant (ATS.ats_analysis_duration ats_hdr_0)
>         filePointer = lfSaw kr (1 / dur) 1 * 0.5 + 0.5
>     in X.atsSynth ar 0 numPartials partialStart partialSkip filePointer freqMul freqAdd

resynthesize only the top half of the partials

> g_01 =
>   let np = constant (ATS.ats_n_partials ats_hdr_0)
>   in f_01 (np / 2) (np / 2) 3 1 0

> f_02 = f_01 (constant (ATS.ats_n_partials ats_hdr_0))

> g_02 = f_02 0 1 1 0 -- default
> g_03 = f_02 0 1 1.5 0 -- multiply frequencies by 1.5
> g_04 = f_02 0 1 1 100 -- add 100 to all frequencies
> g_05 = f_02 0 3 1 0 -- resynthesize every third partial only (partial skip)
