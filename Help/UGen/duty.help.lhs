    > Sound.SC3.UGen.Help.viewSC3Help "Duty"
    > Sound.SC3.UGen.DB.ugenSummary "Duty"

> import Sound.SC3 {- hsc3 -}
>
> g_01 :: UId m => m UGen
> g_01 = do
>   n0 <- drandM dinf (mce [0.01,0.2,0.4])
>   n1 <- dseqM dinf (mce [204,400,201,502,300,200])
>   let f = duty KR n0 0 RemoveSynth n1
>   return (sinOsc AR (f * mce2 1 1.01) 0 * 0.1)

Using control rate signal, mouseX, to determine duration.

> g_02 =
>     let n = dseq 'α' dinf (mce [204,400,201,502,300,200])
>         x = mouseX KR 0.001 2 Linear 0.1
>         f = duty KR x 0 RemoveSynth n
>     in sinOsc AR (f * mce2 1 1.01) 0 * 0.1
