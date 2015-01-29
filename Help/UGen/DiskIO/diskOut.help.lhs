> Sound.SC3.UGen.Help.viewSC3Help "DiskOut"
> Sound.SC3.UGen.DB.ugenSummary "DiskOut"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

Example graph

> let gr = let d = xLine KR 20000 2 10 RemoveSynth
>          in dust 'α' AR d * 0.15

> let gr = soundIn 0

Check incoming signal (either graph above or the outside world)

> audition (out 0 gr)

Record incoming signal (or above...), print some informational traces...

> let trace str = liftIO (putStrLn str)

> withSC3 (do {trace "b_alloc & b_write"
>             ;_ <- async (b_alloc 0 65536 1)
>             ;_ <- async (b_write 0 "/tmp/disk-out.aiff" Aiff PcmInt16 (-1) 0 True)
>             ;trace "record for 10 seconds"
>             ;playSynthdef 2001 (synthdef "disk-out" (diskOut 0 gr))
>             ;pauseThread 10
>             ;trace "stop recording and tidy up"
>             ;send (n_free [2001])
>             ;_ <- async (b_close 0)
>             ;_ <- async (b_free 0)
>             ;return ()})

Listen to recording (on loop...)

> let {fn = "/tmp/disk-out.aiff"
>     ;nc = 1
>     ;gr = out 0 (diskIn nc 0 Loop)}
> in withSC3 (do {_ <- async (b_alloc 0 65536 nc)
>                ;_ <- async (b_read 0 fn 0 (-1) 0 True)
>                ;play gr})
