    Sound.Sc3.Lang.Help.viewServerHelp "/dumpOSC"

> import Sound.OSC {- hosc -}
> import Sound.Sc3 {- hsc3 -}

> ex_01 :: Transport m => m ()
> ex_01 = do
>   sendMessage (dumpOSC TextPrinter)
>   play (out 0 (sinOsc AR (rand 'α' 440 880) 0 * 0.1))
>   pauseThread 1.0
>   reset
>   sendMessage (dumpOSC NoPrinter)

    withSc3 ex_01
    withSc3 (sendMessage (dumpOSC TextPrinter))
