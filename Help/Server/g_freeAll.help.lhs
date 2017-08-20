    > Sound.SC3.Server.Help.viewServerHelp "/g_freeAll"

> import Sound.OSC
> import Sound.SC3

> m1 = g_freeAll [0]

     > withSC3 (sendMessage m1)
