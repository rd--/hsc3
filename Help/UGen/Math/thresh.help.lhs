> Sound.SC3.UGen.Help.viewSC3Help "Operator.thresh"
> :t thresh

> import Sound.SC3.ID

low-rent gate
> let n = lfNoise0 'a' AR 50 * 0.5
> in audition (out 0 (thresh n 0.45))
