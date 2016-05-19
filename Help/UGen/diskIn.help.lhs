    > Sound.SC3.UGen.Help.viewSC3Help "DiskIn"
    > Sound.SC3.UGen.DB.ugenSummary "DiskIn"

> import Sound.SC3 {- hsc3 -}
>
> fn_01 = "/home/rohan/data/audio/pf-c5.snd"
> nc_01 = 1
> msg_01 = [b_alloc 0 65536 nc_01,b_read 0 fn_01 0 (-1) 0 True]
> msg_02 = [b_close 0,b_free 0]

allocate buffer and open file for reading

    > withSC3 (mapM_ async msg_01)

> g_01 = diskIn nc_01 0 Loop

close file & free buffer

    > withSC3 (mapM_ async msg_02)
