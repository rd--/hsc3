> Sound.SC3.UGen.Help.viewSC3Help "SwitchDelay"
> Sound.SC3.UGen.DB.ugenSummary "SwitchDelay"

> import Sound.SC3

simple feedback delay
> audition (out 0 (switchDelay (soundIn 4) 1 1 1 0.99 20))

change the buffer read pointer periodically.
> let {ix = stepper (impulse KR 0.5 0) 0 0 3 1 0
>     ;dt = select ix (mce [0.02,0.1,0.725,0.25])
>     ;sd = switchDelay (soundIn 4) 1 1 dt 0.99 20}
> in audition (out 0 sd)
