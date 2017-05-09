    Sound.SC3.Server.Help.viewServerHelp "/dumpOSC"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

> ex_01 :: Transport m => m ()
> ex_01 = do
>   sendMessage (dumpOSC TextPrinter)
>   play (out 0 (sinOsc AR (rand 'α' 440 880) 0 * 0.1))
>   pauseThread 1.0
>   reset
>   sendMessage (dumpOSC NoPrinter)

    withSC3 ex_01
    withSC3 (sendMessage (dumpOSC TextPrinter))
