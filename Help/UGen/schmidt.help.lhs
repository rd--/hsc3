    Sound.SC3.UGen.Help.viewSC3Help "Schmidt"
    Sound.SC3.UGen.DB.ugenSummary "Schmidt"

> import Sound.SC3

threshold octave jumps

> g_01 =
>     let n = lfNoise1 'α' KR 3
>         o = schmidt n (-0.15) 0.15 + 1
>     in sinOsc AR (n * 200 + 500 * o) 0 * 0.1
