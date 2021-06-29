-- pulseCount
let c = pulseCount (impulse ar 10 0) (impulse ar 0.4 0)
in sinOsc ar (c * 200) 0 * 0.05

-- pulseCount ; printer (silent, see console)
let b = localBuf 'α' 11 1
    t = impulse ar 10 0
    p = pulseCount t 0
    d = demand t 0 (dbufwr 'α' (-666) b p NoLoop)
in mrg [dc ar 0,poll t p 0 (label "p")]
