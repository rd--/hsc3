> import Sound.SC3 {- hsc3 -}

two pi divided by the nominal sample rate (ie. a very small number)

> g_01 =
>     let f = mce2 radiansPerSample ((2 * pi) / sampleRate) * 5e6
>     in sinOsc AR f 0 * 0.1
