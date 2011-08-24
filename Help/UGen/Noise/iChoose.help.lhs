iChoose u
iChoose' l

Select an element of the input mce node at random.

iChoose is a composite of iRand and select.

> import Sound.SC3.ID

> audition (out 0 (sinOsc AR (iChoose' 'a' [440,550,660]) 0 * 0.1))
