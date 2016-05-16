> Sound.SC3.UGen.Help.viewSC3Help "Operator.thresh"
> :t thresh

> import Sound.SC3

low-rent gate

> let n = lfNoise0 'α' AR 50 * 0.5
> in audition (out 0 (thresh n 0.45))
