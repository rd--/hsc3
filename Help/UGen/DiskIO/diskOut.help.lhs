> Sound.SC3.UGen.Help.viewSC3Help "DiskOut"
> Sound.SC3.UGen.DB.ugenSummary "DiskOut"

> import Sound.OSC {- hosc -}
> import Sound.SC3.ID {- hsc3 -}

> let gr = let d = xLine KR 20000 2 10 RemoveSynth
>          in out 0 (dust 'a' AR d * 0.15)

Check incoming signal

> audition (out 0 (soundIn 4))

Record incoming signal (or above...)

> let trace str = liftIO (putStrLn str)

> withSC3 (do {trace "b_alloc & b_write"
>             ;_ <- async (b_alloc 0 65536 1)
>             ;_ <- async (b_write 0 "/tmp/disk-out.aiff" Aiff PcmInt16 0 0 True)
>             ;trace "record for 10 seconds"
>             ;playSynthdef 2001 (synthdef "disk-out" (diskOut 0 (soundIn 4)))
>             ;pauseThread 10
>             ;trace "stop recording and tidy up"
>             ;send (n_free [2001])
>             ;_ <- async (b_close 0)
>             ;_ <- async (b_free 0)
>             ;return ()})
