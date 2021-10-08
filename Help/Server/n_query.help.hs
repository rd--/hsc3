    Sound.SC3.Lang.Help.viewServerHelp "/n_query"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

> d_00 =
>   let f = control KR "freq" 440
>       o = saw AR f * 0.1
>   in synthdef "saw" (out 0 o)

> m_00 = s_new0 "saw" 1000 AddToTail 1

   > withSC3 (async_ (d_recv d_00) >> sendMessage m_00)

> x_01 :: Transport m => m Message
> x_01 = withNotifications $ do
>   sendMessage (n_query [1000])
>   waitReply "/n_info"

    > r <- withSC3 x_01
    > print r
