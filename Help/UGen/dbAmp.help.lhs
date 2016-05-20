    > Sound.SC3.UGen.Help.viewSC3Help "Operator.dbamp"
    > :t dbAmp

> import Sound.SC3 {- hsc3 -}

Linear db motion is exponential amplitude decay

> g_01 =
>     let a = dbAmp (line KR (-12) (-40) 10 RemoveSynth)
>     in fSinOsc AR 800 0 * a

There is a non-UGen variant.

    > dbAmp (-26::Double) == 0.05011872336272722
