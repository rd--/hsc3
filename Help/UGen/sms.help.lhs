    Sound.SC3.UGen.Help.viewSC3Help "SMS"
    Sound.SC3.UGen.DB.ugenSummary "SMS"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

sine reconstruction left channel, noises on right

> g_01 =
>   let z = soundIn 0
>       y = mouseY KR 1 50 Linear 0.2
>       x = mouseX KR 0.5 4 Linear 0.2
>   in X.sms {- AR -} z 50 y 8 0.3 x 0 0 0 1 (-1)

default param

> g_02 =
>   let z = soundIn 0
>   in X.sms {- AR -} z 80 80 4 0.2 1 0 0 0 1 (-1)
