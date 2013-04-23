> Sound.SC3.UGen.Help.viewSC3Help "LPCSynth"
> Sound.SC3.UGen.DB.ugenSummary "LPCSynth"

> import Sound.SC3.ID

> let load_data b i d =
>         if length d < 512
>         then send (b_setn1 b i d)
>         else do {send (b_setn1 b i (take 512 d))
>                 ;load_data b (i + 512) (drop 512 d)}

> :t load_data

> let lpc_instr b n lpc =
>         let {x = mouseX KR 0.05 1.5 Linear 0.2
>             ;y = mouseY KR 0.25 2.0 Linear 0.2
>             ;f = x / constant (lpcAnalysisDuration (lpcHeader lpc))
>             ;ptr = lfSaw AR f 1 * 0.5 + 0.5
>             ;[cps, rms, err] = mceChannels (lpcVals AR b ptr)
>             ;nh = floorE (22000 / cps)
>             ;voc = blip AR (cps * y) nh * (1 - err)
>             ;s = lpcSynth b (voc + (n * err * 20)) ptr}
>         in s * 1e-5 * rms

> do {lpc <- lpcRead "/home/rohan/cvs/tn/tn-56/lpc/fate.lpc"
>    ;let {n = pinkNoise 'a' AR
>         ;d = map realToFrac (lpcSC3 lpc)
>         ;s = lpc_instr 10 n lpc}
>     in withSC3 (do {_ <- async (b_alloc 10 (length d) 1)
>                    ;load_data 10 0 d
>                    ;play (out 0 s)})}
