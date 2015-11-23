> Sound.SC3.Server.Help.viewServerHelp "/n_query"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

> let d = let {f = control KR "freq" 440
>             ;o = saw AR f * 0.1}
>         in synthdef "saw" (out 0 o)

> withSC3 (async (d_recv d) >> send (s_new0 "saw" 1000 AddToTail 1))

> r <- withSC3 (withNotifications (send (n_query [1000]) >> waitReply "/n_info"))

> print r
