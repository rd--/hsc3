> Sound.SC3.UGen.Help.viewSC3Help "SendReply"
> Sound.SC3.UGen.DB.ugenSummary "SendReply"

> import Sound.SC3.ID

> let {s0 = lfNoise0 'a' KR 5
>     ;s1 = lfNoise0 'b' KR 5
>     ;o = sinOsc AR (s0 * 200 + 500) 0 * s1 * 0.1}
> in audition (mrg [sendReply s0 0 "/s-reply" [s0,s1],out 0 o])

> import Sound.OpenSoundControl

> withSC3 (\fd -> do {async fd (notify True)
>                    ;r <- wait fd "/s-reply"
>                    ;putStrLn (show r)
>                    ;async fd (notify False)})
