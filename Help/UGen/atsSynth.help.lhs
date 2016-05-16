> Sound.SC3.UGen.Help.viewSC3Help "AtsSynth"
> Sound.SC3.UGen.DB.ugenSummary "AtsSynth"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.Data.ATS {- hsc3-data -}

> let ats_fn = "/home/rohan/data/audio/pf-c5.4.ats"
> let ats_fn = "/home/rohan/cvs/tn/tn-56/ats/metal.ats"

> ats_load_sc3 0 ats_fn

> withSC3 (b_query1_unpack 0)

read file

> hdr <- fmap ats_header (ats_read ats_fn)

show header

> putStrLn $ ats_header_pp hdr

simple re-synthesiser, ATS data is at buffer 0

> let {x = mouseX KR 0.0 1.0 Linear 0.2
>     ;y = mouseY KR 0.75 1.25 Linear 0.2
>     ;np = constant (ats_n_partials hdr)
>     ;rs = atsSynth 0 np 0 1 x y 0}
> in audition (out 0 rs)
