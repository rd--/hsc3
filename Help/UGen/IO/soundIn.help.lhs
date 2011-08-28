> Sound.SC3.UGen.Help.viewSC3Help "SoundIn"

soundIn is a composite UGen.

> import Sound.SC3

> audition (out 0 (soundIn 0))
> audition (out 0 (soundIn (mce2 0 1)))
> audition (out 0 (soundIn (mce [0, 2, 1, 3])))
