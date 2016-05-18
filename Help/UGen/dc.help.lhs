    > Sound.SC3.UGen.Help.viewSC3Help "DC"
    > Sound.SC3.UGen.DB.ugenSummary "DC"

> import Sound.SC3 {- hsc3 -}

error...

> g_01 = 0 :: UGen

    > withSC3 (send (n_trace [-1]))

zero

> g_02 = dc AR 0

DC offset; will click on start and finish

> g_03 = dc AR 0.5
> g_04 = 0.5 + sinOsc AR 440 0 * 0.1
> g_05 = dc AR 0.5 + sinOsc AR 440 0 * 0.1

Transient before LeakDC adapts and suppresses the offset?

> g_06 = dc AR 1
> g_07 = leakDC (dc AR 1) 0.995
