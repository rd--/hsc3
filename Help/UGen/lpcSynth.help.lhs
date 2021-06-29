> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> import qualified Sound.SC3.Data.LPC as LPC {- hsc3-data -}

> lpc_instr buf noise lpc =
>   let x = mouseX kr 0.01 1.5 Linear 0.2
>       y = mouseY kr 0.25 2.0 Linear 0.2
>       f = x / constant (LPC.lpcAnalysisDuration (LPC.lpcHeader lpc))
>       ptr = lfSaw ar f 1 * 0.5 + 0.5
>       [cps, rms, err] = mceChannels (X.lpcVals ar buf ptr)
>       nh = floorE (22000 / cps)
>       voc = blip ar (cps * y) nh * (1 - err)
>   in X.lpcSynth buf (voc + (noise * err)) ptr * rms

> fn_01 = "/home/rohan/sw/hsc3-data/data/lpc/fate.lpc"
> fn_02 = "/home/rohan/uc/invisible/clarity/lpc/z.01.lpc"

> au_01 lpc noise = do
>   let d = map realToFrac (LPC.lpcSC3 lpc)
>   _ <- async (b_alloc 10 (length d) 1)
>   mapM_ sendMessage (b_setn1_segmented 512 10 0 d)
>   let s = lpc_instr 10 noise lpc
>   play (out 0 s)

    lpc <- LPC.lpc_read_binary LPC.BigEndian fn_01
    lpc <- LPC.lpc_read_text fn_02
    withSC3 (au_01 lpc (whiteNoise 'α' ar * 10))
