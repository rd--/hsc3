soundIn channel

Read audio from the sound input hardware.

channel - input channel number to read, 
          indexed from zero, can be mce.

> import Sound.SC3

> audition (out 0 (soundIn 0))

> audition (out 0 (soundIn (mce2 0 1)))

> audition (out 0 (soundIn (mce [0, 2, 1, 3])))
