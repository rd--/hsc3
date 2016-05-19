    > Sound.SC3.UGen.Help.viewSC3Help "ReplaceOut"
    > Sound.SC3.UGen.DB.ugenSummary "ReplaceOut"

> import Sound.SC3 {- hsc3 -}

> g_01 = replaceOut 0 (sinOsc AR 440 0 * 0.1)

Send signal to a bus, overwrite existing signal.

> g_02 =
>     let a = out 0 (sinOsc AR (mce [330, 331]) 0 * 0.1)
>         b = replaceOut 0 (sinOsc AR (mce [880, 881]) 0 * 0.1)
>         c = out 0 (sinOsc AR (mce [120, 121]) 0 * 0.1)
>     in mrg [a, b, c]

Compare to

> g_03 =
>     let a = out 0 (sinOsc AR (mce [330, 331]) 0 * 0.1)
>         b = out 0 (sinOsc AR (mce [880, 881]) 0 * 0.1)
>         c = out 0 (sinOsc AR (mce [120, 121]) 0 * 0.1)
>     in mrg [a,b,c]

- a writes noise to 24
- b reads 24 and replaces with filtered variant
- c reads 24 and writes to 0

> g_04 =
>     let a = out 24 (pinkNoise 'α' AR * 0.1)
>         b = replaceOut 24 (bpf (in' 1 AR 24) 440 1)
>         c = out 0 (in' 1 AR 24)
>     in mrg [c,b,a]

    > putStrLn$ synthstat g_04

~~~~
number of constants       : 5
number of controls        : 0
control rates             : []
number of unit generators : 7
unit generator rates      : [(AR,7)]
unit generator set        : *,BPF,In,Out,PinkNoise,ReplaceOut
unit generator sequence   : PinkNoise,*,Out,In,BPF,ReplaceOut,Out
~~~~
