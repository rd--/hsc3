    > Sound.SC3.UGen.Help.viewSC3Help "SinOscFB"
    > Sound.SC3.UGen.DB.ugenSummary "SinOscFB"

> import Sound.SC3 {- hsc3 -}

    {SinOscFB.ar([400,301],MouseX.kr(0,4))*0.1}.play

> g_01 =
>     let x = mouseX KR 0 4 Linear 0.2
>     in sinOscFB AR (mce2 400 301) x * 0.1

    {var y = MouseY.kr(10,1000,'exponential')
    ;var x = MouseX.kr(0.5pi,pi)
    ;SinOscFB.ar(y,x) * 0.1}.play

> g_02 =
>     let y = mouseY KR 10 1000 Exponential 0.2
>         x = mouseX KR (pi/2) pi Linear 0.2
>     in sinOscFB AR y x * 0.1

    {var y = MouseY.kr(1,1000,'exponential')
    ;var x = MouseX.kr(0.5pi,pi)
    ;SinOscFB.ar(100 * SinOscFB.ar(y)+200,x) * 0.1}.play

> g_03 =
>     let y = mouseY KR 1 1000 Exponential 0.2
>         x = mouseX KR (pi/2) pi Linear 0.2
>     in sinOscFB AR (100 * sinOscFB AR y 0 + 200) x * 0.1
