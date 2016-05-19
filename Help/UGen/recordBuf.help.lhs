    > Sound.SC3.UGen.Help.viewSC3Help "RecordBuf"
    > Sound.SC3.UGen.DB.ugenSummary "RecordBuf"

> import Sound.SC3 {- hsc3 -}

Allocate a buffer (assume SR of 48k)

    > withSC3 (async (b_alloc 0 (48000 * 4) 1))

Record for four seconds (until end of buffer)

> g_01 =
>     let o = formant AR (xLine KR 400 1000 4 DoNothing) 2000 800 * 0.125
>     in mrg2 o (recordBuf AR 0 0 1 0 1 NoLoop 1 RemoveSynth o)

Play it back

> g_02 = playBuf 1 AR 0 1 1 0 NoLoop RemoveSynth

Mix second signal equally with existing signal, replay to hear

> g_03 =
>     let o = formant AR (xLine KR 200 1000 4 DoNothing) 2000 800 * 0.125
>     in mrg2 o (recordBuf AR 0 0 0.5 0.5 1 NoLoop 1 RemoveSynth o)
