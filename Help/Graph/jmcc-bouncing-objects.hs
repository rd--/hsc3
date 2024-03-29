-- bouncing objects (jmcc) #2 ; texture=spawn,1,inf
let imp_frq = xLine kr (5 + rand (-2) 2) 600 4 DoNothing
    imp_amp = xLine kr 0.09 0.000009 4 DoNothing
    imp = impulse ar imp_frq 0 * imp_amp
    exc = decay imp 0.001
    flt_frq = X.randN 4 400 8400
    flt_amp = X.randN 4 0 1
    flt_rtm = X.randN 4 0.01 0.11
    flt = klank exc 1 0 1 (klankSpec_mce flt_frq flt_amp flt_rtm)
    loc = pan2 flt (rand (-1) 1) 1
    e = Envelope [1,1,0] [3,0.001] (replicate 2 EnvLin) Nothing Nothing 0
in loc * envGen kr 1 1 0 1 RemoveSynth e

-- bouncing objects (jmcc) #2 ; texture=spawn,1,inf ; id
let imp_frq = xLine kr (5 + randId 'α' (-2) 2) 600 4 DoNothing
    imp_amp = xLine kr 0.09 0.000009 4 DoNothing
    imp = impulse ar imp_frq 0 * imp_amp
    exc = decay imp 0.001
    flt_frq = X.randNId 4 'β' 400 8400
    flt_amp = X.randNId 4 'γ' 0 1
    flt_rtm = X.randNId 4 'δ' 0.01 0.11
    flt = klank exc 1 0 1 (klankSpec_mce flt_frq flt_amp flt_rtm)
    loc = pan2 flt (randId 'ε' (-1) 1) 1
    e = Envelope [1,1,0] [3,0.001] (replicate 2 EnvLin) Nothing Nothing 0
in loc * envGen kr 1 1 0 1 RemoveSynth e
