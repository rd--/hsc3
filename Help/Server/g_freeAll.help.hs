    Sound.Sc3.Lang.Help.viewServerHelp "/g_freeAll"

> import Sound.OSC
> import Sound.Sc3

> m1 = g_freeAll [0]

     > withSc3 (sendMessage m1)
