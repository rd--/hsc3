/b_getn                               Get ranges of sample value(s)

int   - buffer number
[
  int - starting sample index
  int - number of sequential samples to get (M)
] * N

Get contiguous ranges of samples. Replies to sender with the
corresponding /b_setn command. This is only meant for getting a few
samples, not whole buffers or large sections.

> import Sound.OpenSoundControl
> import Sound.SC3
> import Sound.SC3.Plot

> let fn = "/home/rohan/data/audio/pf-c5.aif"
> in withSC3 (\fd -> async fd (b_allocRead 0 fn 0 0))

> d <- withSC3 (\fd -> b_getn1_data_segment fd 1024 0 (0,2^15))
> plotTable [d]
