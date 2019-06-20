     Sound.SC3.Lang.Help.viewServerHelp "/n_trace"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

> m_01 =
>   [d_recv defaultSynthdef
>   ,s_new "default" 100 AddToHead 1 []]

> m_02 = n_trace [1,100]

    > withSC3 (mapM_ maybe_async m_01)
    > withSC3 (sendMessage m_02)
