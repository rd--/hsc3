> import Sound.SC3 {- hsc3 -}

> e_01 :: (Fractional n,Ord n) => Envelope n
> e_01 =
>   let l = [0,0.6,0.3,1.0,0]
>       t = [0.1,0.02,0.4,1.1]
>       c = [EnvLin,EnvExp,EnvNum (-6),EnvSin]
>   in Envelope l t c Nothing Nothing 0

    import Sound.SC3.Plot {- hsc3-plot -}
    plotEnvelope [e_01]

> g_01 =
>     let x = mouseX KR 0 (envelope_duration e_01) Linear 0.2
>         g = iEnvGen KR x e_01
>     in sinOsc AR (g * 500 + 440) 0 * 0.1

index with SinOsc. mouse controls amplitude of SinOsc.
use offset so negative values of SinOsc will map into the Env

> e_02 :: (Fractional n,Ord n) => Envelope n
> e_02 =
>   let l = [-1,-0.7,0.7,1]
>       t = [0.8666,0.2666,0.8668]
>       c = [EnvLin,EnvLin]
>   in Envelope l t c Nothing Nothing 0

    plotEnvelope [e_02]

> g_02 =
>     let x = mouseX KR 0 1 Linear 0.2
>         o = (sinOsc AR 440 0 + 1) * x
>     in iEnvGen AR o e_02 * 0.1

index with Amplitude of input, control freq of SinOsc (uses SoundIn)

> e_03 :: (Fractional n,Ord n) => Envelope n
> e_03 = envXYC [(0, 330, EnvExp), (0.5, 440, EnvExp), (1.0, 1760, EnvLin)]

    plotEnvelope [e_03]

> g_03 =
>     let pt = amplitude AR (soundIn 0) 0.01 0.2
>     in sinOsc AR (iEnvGen KR pt e_03) 0 * 0.2
