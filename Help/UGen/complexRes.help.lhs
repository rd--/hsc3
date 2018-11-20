    Sound.SC3.UGen.Help.viewSC3Help "ComplexRes"
    Sound.SC3.UGen.DB.ugenSummary "ComplexRes"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as External {- hsc3 -}

> g_01 =
>   let s = pulse AR 0.1 0.001 * 0.1
>       fr = 50 + 5000 * sinOsc AR 50 0
>       dt = 0.5
>   in External.complexRes s fr dt
