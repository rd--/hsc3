> :t choose

# composite
choose is a composite of iRand and select.

> import Sound.SC3.ID

> let f = uclone 'a' 2 (choose 'a' (mce [440,460 .. 880]))
> in audition (out 0 (sinOsc AR f  0 * 0.1))
