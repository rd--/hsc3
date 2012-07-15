> Sound.SC3.UGen.Help.viewSC3Help "SoundIn"

# composite

> import Sound.SC3

Copy 5th input channel (index 4) to 1st output channel (index 0).
> audition (out 0 (soundIn 4))

Copy input from 4 & 5 to 0 & 1.
> audition (out 0 (soundIn (mce2 4 5)))

IO matrix:    0 1 2 3
            0 *
            1     *
            2   *
            3       *
> audition (out 0 (soundIn (mce [0, 2, 1, 3])))
