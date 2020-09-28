    Sound.SC3.Lang.Help.viewSC3Help "PV_JensenAndersen"
    Sound.SC3.UGen.DB.ugenSummary "PV_JensenAndersen"

> import Sound.SC3 {- hsc3 -}

buffer propsc=0.25 prophfe=0.25 prophfc=0.25 propsf=0.25 threshold=1.0 waittime=0.04

> g_01 =
>     let i = soundIn 0
>         f = fft' (localBuf 'α' 2048 1) i
>         x = mouseX KR 0.01 1.0 Linear 0.2
>         h = pv_JensenAndersen f 0.25 0.25 0.25 0.25 x 0.04
>     in sinOsc AR (mrg2 440 445) 0 * decay (h * 0.1) 0.1 * 0.1
