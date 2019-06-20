    Sound.SC3.Lang.Help.viewServerHelp "/status"

> import Sound.SC3 {- hsc3 -}

    withSC3 serverStatus >>= mapM putStrLn
