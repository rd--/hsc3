-- gVerb ; mono reverb
let i = impulse ar (mce2 1 2) 0
    c = lfCub ar (mce2 900 1200) 0
    s = decay i (mce2 0.05 0.25) * c * 0.05
in mix (gVerb s 10 3 0.5 0.5 15 1 0.7 0.5 300)

-- gVerb ; controls
let roomsize = control kr "roomsize" 10.0
    revtime = control kr "revtime" 3.0
    damping = control kr "damping" 0.5
    inputbw = control kr "inputbw" 0.5
    spread = control kr "spread" 15.0
    drylevel = control kr "drylevel" 1.0
    earlyreflevel = control kr "earlyreflevel" 0.7
    taillevel = control kr "taillevel" 0.5
    maxroomsize = control kr "maxroomsize" 300.0
in gVerb (soundIn 0) roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize
