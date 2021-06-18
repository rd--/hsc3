-- gVerb ; mono reverb
let i = impulse AR (mce2 1 2) 0
    c = lfCub AR (mce2 900 1200) 0
    s = decay i (mce2 0.05 0.25) * c * 0.05
in mix (gVerb s 10 3 0.5 0.5 15 1 0.7 0.5 300)

-- gVerb ; controls
let roomsize = control KR "roomsize" 10.0
    revtime = control KR "revtime" 3.0
    damping = control KR "damping" 0.5
    inputbw = control KR "inputbw" 0.5
    spread = control KR "spread" 15.0
    drylevel = control KR "drylevel" 1.0
    earlyreflevel = control KR "earlyreflevel" 0.7
    taillevel = control KR "taillevel" 0.5
    maxroomsize = control KR "maxroomsize" 300.0
in gVerb (soundIn 0) roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize
