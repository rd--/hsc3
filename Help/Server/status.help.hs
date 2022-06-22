    Sound.Sc3.Lang.Help.viewServerHelp "/status"

> import Sound.Sc3 {- hsc3 -}

    withSc3 serverStatus >>= mapM putStrLn
