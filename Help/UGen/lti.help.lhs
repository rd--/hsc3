    Sound.SC3.UGen.Help.viewSC3Help "LTI"
    Sound.SC3.UGen.DB.ugenSummary "LTI"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> g_01 =
>     let a = [0.02,-0.01]
>         b = [1,0.7,0,0,0,0,-0.8,0,0,0,0,0.9,0,0,0,-0.5,0,0,0,0,0,0,0.25,0.1,0.25]
>         z = pinkNoise 'α' AR * 0.1
>     in X.lti AR z (asLocalBuf 'β' a) (asLocalBuf 'γ' b)
