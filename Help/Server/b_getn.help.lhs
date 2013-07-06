> Sound.SC3.Server.Help.viewServerHelp "/b_getn"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.Plot {- hsc3-plot -}

Allocate and generate wavetable buffer (256 frames)
> withSC3 (do {_ <- async (b_alloc 0 256 1)
>             ;let f = [Normalise,Clear]
>              in send (b_gen_sine1 0 f [1,1/2,1/3,1/4,1/5])})

Run simple read and draw buffer
> d0 <- withSC3 (b_getn1_data 0 (0,255))
> plotTable [d0]

Load sound file
> let fn = "/home/rohan/data/audio/pf-c5.aif"
> in withSC3 (async (b_allocRead 1 fn 0 0))

Run segmented read (2^15 frames in 1024 frame segments) and draw buffer
> d1 <- withSC3 (b_getn1_data_segment 1024 1 (0,2^15))
> plotTable [d1]

