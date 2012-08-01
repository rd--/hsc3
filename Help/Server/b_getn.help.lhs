> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.Plot {- hsc3-plot -}

> let fn = "/home/rohan/data/audio/pf-c5.aif"
> in withSC3 (async (b_allocRead 0 fn 0 0))

> d <- withSC3 (b_getn1_data_segment 1024 0 (0,2^15))
> plotTable [d]
