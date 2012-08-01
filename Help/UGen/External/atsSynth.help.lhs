> Sound.SC3.UGen.Help.viewSC3Help "AtsSynth"
> Sound.SC3.UGen.DB.ugenSummary "AtsSynth"

> import Sound.SC3

read file
> ats <- atsRead "/home/rohan/cvs/tn/tn-56/ats/metal.ats"

show header
> atsHeader ats

data loader that works in segments (udp packet limits)
> let load_data b i d =
>         if length d < 512
>         then send (b_setn1 b i d)
>         else do {send (b_setn1 b i (take 512 d))
>                 ;load_data b (i + 512) (drop 512 d)}

simple re-synthesiser
> let {h = atsHeader ats
>     ;d = atsData ats
>     ;x = mouseX KR 0.05 1.5 Linear 0.2
>     ;y = mouseY KR 0.25 2.0 Linear 0.2
>     ;np = constant (atsNPartials h)
>     ;f = x / constant (atsAnalysisDuration h)
>     ;ptr = lfSaw AR f 1 * 0.5 + 0.5
>     ;rs = atsSynth 10 np 0 1 (clip ptr 0 1) y 0}
> in withSC3 (do {_ <- async (b_alloc 10 (length d) 1)
>                ;load_data 10 0 d
>                ;play (out 0 rs)})
