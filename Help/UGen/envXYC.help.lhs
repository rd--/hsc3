    Sound.SC3.UGen.Help.viewSC3Help "Env.*xyc"

> import Sound.SC3 {- hsc3 -}

> e_01 :: (Fractional n,Ord n) => Envelope n
> e_01 = envXYC [(0, 330, EnvExp), (0.5, 440, EnvExp), (1.0, 1760, EnvLin)]

    import Sound.SC3.Plot {- hsc3-plot -}
    plotEnvelope [e_01]
