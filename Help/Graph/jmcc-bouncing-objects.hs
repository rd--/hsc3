-- bouncing objects (jmcc) #2 ; texture=spawn,1,inf
let imp_frq = xLine KR (5 + rand 'α' (-2) 2) 600 4 DoNothing
    imp_amp = xLine KR 0.09 0.000009 4 DoNothing
    imp = impulse AR imp_frq 0 * imp_amp
    exc = decay imp 0.001
    flt_frq = X.rRandN 4 'β' 400 8400
    flt_amp = X.rRandN 4 'γ' 0 1
    flt_rtm = X.rRandN 4 'δ' 0.01 0.11
    flt = klank exc 1 0 1 (klankSpec_mce flt_frq flt_amp flt_rtm)
    loc = pan2 flt (rand 'ε' (-1) 1) 1
    e = Envelope [1,1,0] [3,0.001] (replicate 2 EnvLin) Nothing Nothing 0
in loc * envGen KR 1 1 0 1 RemoveSynth e
