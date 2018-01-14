    > Sound.SC3.UGen.Help.viewSC3Help "BufSampleRate"
    > Sound.SC3.UGen.DB.ugenSummary "BufSampleRate"

> import Sound.SC3 {- hsc3 -}

Load sound file to buffer zero (required for examples)

> fn_01 = "/home/rohan/data/audio/pf-c5.aif"

> m_01 = b_allocRead 0 fn_01 0 0

    > withSC3 (async m_01)

Sine tone derived from sample rate of buffer (ie. 48000 / 100 == 480) and a 440Hz tone.

> g_01 =
>     let f = mce [bufSampleRate KR 0 * 0.01, 440]
>     in sinOsc AR f 0 * 0.1
