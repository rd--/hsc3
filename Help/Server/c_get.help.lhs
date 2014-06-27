> Sound.SC3.Server.Help.viewServerHelp "/c_get"

> import Sound.OSC {- hosc -}
> import Sound.SC3.ID {- hsc3 -}

> audition (out 0 (tRand 'α' 220 2200 (dust 'β' KR 1)))

> withSC3 (do {send (c_get [0])
>             ;r <- waitReply "/c_set"
>             ;liftIO (print r)})
