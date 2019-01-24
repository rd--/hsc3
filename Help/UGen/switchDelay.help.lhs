    Sound.SC3.UGen.Help.viewSC3Help "SwitchDelay"
    Sound.SC3.UGen.DB.ugenSummary "SwitchDelay"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

simple feedback delay

> g_01 = X.switchDelay (soundIn 0) 1 1 1 0.99 20

> g_02 = X.switchDelay (soundIn 0) 1 0.7 0.4 0.6 20

change the buffer read pointer periodically.

> g_03 =
>   let ix = stepper (impulse KR 0.5 0) 0 0 3 1 0
>       dt = select ix (mce [0.02,0.1,0.725,0.25])
>   in X.switchDelay (soundIn 0) 1 0.6 dt 0.7 20
