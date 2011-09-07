> Sound.SC3.UGen.Help.viewSC3Help "AtsNoiSynth"
> Sound.SC3.UGen.DB.ugenSummary "AtsNoiSynth"

> import Sound.SC3

segmented file loader
> let load_data fd b i d =
>         if length d < 512
>         then send fd (b_setn1 b i d)
>         else do {send fd (b_setn1 b i (take 512 d))
>                 ;load_data fd b (i + 512) (drop 512 d)}

read file
> ats <- atsRead "/home/rohan/cvs/tn/tn-56/ats/metal.ats"

run re-synthesis
> let {d = atsData ats
>     ;h = atsHeader ats
>     ;x = mouseX' KR 0.05 1.5 Linear 0.2
>     ;y = mouseY' KR 0 1 Linear 0.2
>     ;np = constant (atsNPartials h)
>     ;f = x / constant (atsAnalysisDuration h)
>     ;ptr = clip (lfSaw AR f 1 * 0.5 + 0.5) 0 1
>     ;rs = atsNoiSynth 10 np 0 1 ptr (1 - y) y 1 0 25 0 1}
> in withSC3 (\fd -> do {async fd (b_alloc 10 (length d) 1)
>                       ;load_data fd 10 0 d
>                       ;play fd (out 0 rs)})
