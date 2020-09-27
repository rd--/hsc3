c.f. equal_to

> import Sound.SC3 {- hsc3 -}

trigger an envelope

> g_01 =
>   let e = envGen KR (sinOsc AR 1 0 `greater_than` 0) 1 0 1 DoNothing (envPerc 0.01 1)
>   in sinOsc AR 440 0 * e * 0.1

