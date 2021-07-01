-- envFollow
let z = soundIn 0
    d = mouseX kr 0.990 0.999 Linear 0.2
    c = X.envFollow kr z d
    o = pinkNoiseId 'α' ar * c
in mce2 z o
