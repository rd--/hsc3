    Sound.SC3.UGen.Help.viewSC3Help "SendTrig"
    Sound.SC3.UGen.DB.ugenSummary "SendTrig"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let s = lfNoise0 'α' KR 5
>         o = sinOsc AR (s * 200 + 500) 0 * 0.1
>     in mrg [o,sendTrig s 0 s]

Retrieve a single message

> f_01 :: (DuplexOSC m) => m Message
> f_01 = withNotifications (waitReply "/tr")

    withSC3 f_01
