-- gateReset
let gt = 1
    tr = trig1 (dust 'α' kr 1) 0
    e = envGen kr (gateReset gt tr) 1 0 1 DoNothing (envPerc 0.01 1)
in sinOsc ar 440 0 * e * 0.1

