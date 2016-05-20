    > Sound.SC3.UGen.Help.viewSC3Help "Operator.thresh"
    > :t thresh

> import Sound.SC3 {- hsc3 -}

low-rent gate

> g_01 = let n = lfNoise0 'α' AR 50 * 0.5 in thresh n 0.45
