> Sound.SC3.UGen.Help.viewSC3Help "abs"
> :t abs

> import Sound.SC3

> audition (out 0 (abs (syncSaw AR 100 440 * 0.1)))
