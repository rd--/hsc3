    Sound.SC3.UGen.Help.viewSC3Help "GreyholeRaw"
    Sound.SC3.UGen.DB.ugenSummary "GreyholeRaw"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

default values

> g_01 =
>   let (i1,i2) = (soundIn 0,soundIn 1)
>   in greyholeRaw i1 i2 0.0 2.0 0.5 0.9 0.1 2.0 1.0
