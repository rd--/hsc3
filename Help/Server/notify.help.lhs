     Sound.SC3.Server.Help.viewServerHelp "/notify"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

> g_01 = sinOsc AR (rand 'α' 440 880) 0 * 0.1

> s_01 = synthdef "g" (out 0 g_01)

> m_01 = s_new "g" (-1) AddToHead 1 []

> f_01 :: DuplexOSC m => m Message
> f_01 = do
>   async_ (d_recv s_01)
>   withNotifications (sendMessage m_01 >> waitReply "/n_go")

    withSC3 (f_01 >>= \r -> liftIO (print r))
