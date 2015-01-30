> Sound.SC3.UGen.Help.viewSC3Help "PulseCount"
> Sound.SC3.UGen.DB.ugenSummary "PulseCount"

> import Sound.SC3 {- hsc3 -}

> let c = pulseCount (impulse AR 10 0) (impulse AR 0.4 0)
> in audition (out 0 (sinOsc AR (c * 200) 0 * 0.05))

printer

> let {b = localBuf 'α' 11 1
>     ;t = impulse AR 10 0
>     ;p = pulseCount t 0
>     ;d = demand t 0 (dbufwr 'α' (-666) b p NoLoop)}
> in audition (mrg [out 0 (dc AR 0),poll t p (label "p") 0])
