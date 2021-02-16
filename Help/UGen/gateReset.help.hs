-- gateReset
let gt = 1
    tr = trig1 (dust 'α' KR 1) 0
    e = envGen KR (gateReset gt tr) 1 0 1 DoNothing (envPerc 0.01 1)
in sinOsc AR 440 0 * e * 0.1

