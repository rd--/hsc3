    Sound.SC3.UGen.Help.viewSC3Help "XLine"
    Sound.SC3.UGen.DB.ugenSummary "XLine"

Note: SC3 reorders mul and add inputs to precede the doneAction input.

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let f = xLine KR 200 17000 10 RemoveSynth
>     in sinOsc AR f 0 * 0.1
