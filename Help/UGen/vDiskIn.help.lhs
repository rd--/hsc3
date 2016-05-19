    > Sound.SC3.UGen.Help.viewSC3Help "VDiskIn"
    > Sound.SC3.UGen.DB.ugenSummary "VDiskIn"

> import Sound.SC3 {- hsc3 -}
>
> fn_01 = "/home/rohan/data/audio/pf-c5.snd"
> nc_01 = 1
> msg_01 = [b_alloc 0 8192 nc_01,b_read 0 fn_01 0 (-1) 0 True]
> msg_02 = [b_close 0,b_free 0]

    > withSC3 (mapM_ async msg_01)

> g_01 = vDiskIn nc_01 0 (sinOsc KR 0.25 0 * 0.25 + 1) Loop 0

    > withSC3 (mapM_ async msg_02)
