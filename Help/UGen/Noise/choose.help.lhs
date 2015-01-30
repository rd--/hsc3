> :t choose

# choose is a composite of iRand and select.

> import Sound.SC3

> let f = uclone 'α' 2 (choose 'β' (mce [440,460 .. 880]))
> in audition (out 0 (sinOsc AR f  0 * 0.1))
