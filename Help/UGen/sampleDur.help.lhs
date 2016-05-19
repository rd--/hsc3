    > Sound.SC3.UGen.Help.viewSC3Help "SampleDur"
    > Sound.SC3.UGen.DB.ugenSummary "SampleDur"

> import Sound.SC3 {- hsc3 -}

`sampleDur` is the reciprocal of the nominal sample rate of the server

> g_01 =
>     let f = mce2 sampleRate (recip sampleDur) * 0.01
>     in sinOsc AR f 0 * 0.1
