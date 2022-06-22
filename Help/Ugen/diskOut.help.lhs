> import Sound.OSC {- hosc -}
> import Sound.Sc3 {- hsc3 -}

Example graph

> g_01 =
>     let d = xLine kr 20000 2 10 RemoveSynth
>     in dustId 'α' ar d * 0.15

> g_02 = soundIn 0

Check incoming signal (either graph above or the outside world, or `C-cC-a`)

    audition (out 0 g_01)

Record incoming signal (or above...), print some informational traces...

> trace str = liftIO (putStrLn str)
> sy_01 = synthdef "disk-out" (diskOut 0 g_01)
> fn_01 = "/tmp/disk-out.aiff"
> nc_01 = 1
> msg_01 = [b_alloc 0 65536 nc_01,b_write 0 fn_01 Aiff PcmInt16 (-1) 0 True]
> msg_02 = [b_close 0,b_free 0]

> act_01 :: Transport m => m ()
> act_01 = do
>   trace "b_alloc & b_write"
>   mapM_ async msg_01
>   trace "record for 10 seconds"
>   playSynthdef (2001,AddToTail,1,[]) sy_01
>   pauseThread 10
>   trace "stop recording and tidy up"
>   sendMessage (n_free [2001])
>   mapM_ async msg_02
>   return ()

    withSc3 act_01

Listen to recording (on loop...)

> msg_03 = [b_alloc 0 65536 nc_01,b_read 0 fn_01 0 (-1) 0 True]

    withSc3 (mapM_ async msg_03)

> g_03 = diskIn nc_01 0 Loop

Tidy up...

    withSc3 (mapM_ async msg_02)
