    Sound.SC3.UGen.Help.viewSC3Help "IEnvGen"
    Sound.SC3.UGen.DB.ugenSummary "IEnvGen"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let l = [0,0.6,0.3,1.0,0]
>         t = [0.1,0.02,0.4,1.1]
>         c = [EnvLin,EnvExp,EnvNum (-6),EnvSin]
>         e = Envelope l t c Nothing Nothing
>         x = mouseX KR 0 (sum t) Linear 0.2
>         g = iEnvGen KR x e
>     in sinOsc AR (g * 500 + 440) 0 * 0.1

index with an SinOsc ... mouse controls amplitude of SinOsc
use offset so negative values of SinOsc will map into the Env

> g_02 =
>     let l = [-1,-0.7,0.7,1]
>         t = [0.8666,0.2666,0.8668]
>         c = [EnvLin,EnvLin]
>         e = Envelope l t c Nothing Nothing
>         x = mouseX KR 0 1 Linear 0.2
>         o = (sinOsc AR 440 0 + 1) * x
>     in iEnvGen AR o e * 0.1
