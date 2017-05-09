    Sound.SC3.UGen.Help.viewSC3Help "SendReply"
    Sound.SC3.UGen.DB.ugenSummary "SendReply"

> import Sound.OSC {- hosc3 -}
> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let s0 = lfNoise0 'α' KR 5
>       s1 = lfNoise0 'β' KR 5
>       o = sinOsc AR (s0 * 200 + 500) 0 * s1 * 0.1
>   in mrg [o,sendReply s0 0 "/send-reply" [s0,s1]]

> f_01 :: (DuplexOSC m) => m Message
> f_01 = withNotifications (waitReply "/send-reply")

    withSC3 f_01
