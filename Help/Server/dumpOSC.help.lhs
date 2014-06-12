> Sound.SC3.Server.Help.viewServerHelp "/dumpOSC"

> import Sound.SC3.ID

> withSC3 (send (dumpOSC TextPrinter))
> audition (out 0 (sinOsc AR (rand 'α' 440 880) 0 * 0.1))
> withSC3 reset
> withSC3 (send (dumpOSC NoPrinter))
