    Sound.SC3.UGen.Help.viewSC3Help "LPCSynth"
    Sound.SC3.UGen.DB.ugenSummary "LPCSynth"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> import qualified Sound.SC3.Data.LPC as LPC {- hsc3-data -}

> lpc_instr buf noise lpc =
>   let x = mouseX KR 0.01 1.5 Linear 0.2
>       y = mouseY KR 0.25 2.0 Linear 0.2
>       f = x / constant (LPC.lpcAnalysisDuration (LPC.lpcHeader lpc))
>       ptr = lfSaw AR f 1 * 0.5 + 0.5
>       [cps, rms, err] = mceChannels (X.lpcVals AR buf ptr)
>       nh = floorE (22000 / cps)
>       voc = blip AR (cps * y) nh -- * (1 - err)
>   in X.lpcSynth buf (voc + (noise * err)) ptr * rms

> fn_01 = "/home/rohan/sw/hsc3-data/data/lpc/fate.lpc"

> au_01 lpc noise = do
>   let d = map realToFrac (LPC.lpcSC3 lpc)
>   _ <- async (b_alloc 10 (length d) 1)
>   mapM_ sendMessage (b_setn1_segmented 512 10 0 d)
>   let s = lpc_instr 10 noise lpc
>   play (out 0 s)

    lpc <- LPC.lpcRead fn_01
    withSC3 (au_01 lpc (pinkNoise 'α' AR * 2.5))
