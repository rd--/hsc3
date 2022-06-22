     Sound.Sc3.Lang.Help.viewServerHelp "/n_trace"

> import Sound.OSC {- hosc -}
> import Sound.Sc3 {- hsc3 -}

> m_01 =
>   [d_recv defaultSynthdef
>   ,s_new "default" 100 AddToHead 1 []]

> m_02 = n_trace [1,100]

    > withSc3 (mapM_ maybe_async m_01)
    > withSc3 (sendMessage m_02)
