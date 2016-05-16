    Sound.SC3.UGen.Help.viewSC3Help "ExpRand"
    Sound.SC3.UGen.DB.ugenSummary "ExpRand"

> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let a = line KR 0.5 0 0.01 RemoveSynth
>         f = expRand 'α' 100.0 8000.0
>     in fSinOsc AR f 0 * a
