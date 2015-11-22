> Sound.SC3.UGen.Help.viewSC3Help "AtsNoiSynth"
> Sound.SC3.UGen.DB.ugenSummary "AtsNoiSynth"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.Data.ATS {- hsc3-data -}

> let ats_fn = "/home/rohan/data/audio/pf-c5.4.ats"
> let ats_fn = "/home/rohan/cvs/tn/tn-56/ats/metal.ats"

load ATS file at scsynth

> ats_load_sc3 0 ats_fn

> withSC3 (b_query1_unpack 0)

read ATS header

> hdr <- fmap ats_header (ats_read ats_fn)

> putStrLn $ ats_header_pp hdr

run re-synthesis

> let {np = constant (ats_n_partials hdr)
>     ;ptr = lfSaw KR (constant (1 / ats_analysis_duration hdr)) 1 * 0.5 + 0.5
>     ;rs = atsNoiSynth 0 np 0 1 ptr 1 0.1 1 0 25 0 1}
> in audition (out 0 rs)

> let {x = mouseX KR 0.0 1.0 Linear 0.2
>     ;y = mouseY KR 0.0 1.0 Linear 0.2
>     ;np = constant (ats_n_partials hdr)
>     ;rs = atsNoiSynth 0 np 0 1 x (1 - y) y 1 0 25 0 1}
> in audition (out 0 rs)
