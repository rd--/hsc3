     Sound.SC3.Lang.Help.viewServerHelp "/c_get"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

> g_01 = out 0 (tRand 'α' 220 2200 (dust 'β' KR 1))

> x_01 :: Transport m => m ()
> x_01 = do
>   sendMessage (c_get [0])
>   r <- waitReply "/c_set"
>   liftIO (print r)

    > withSC3 x_01
