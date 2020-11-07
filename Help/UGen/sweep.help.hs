-- sweep ; using sweep to modulate sine frequency
let x = mouseX KR 0.5 20 Exponential 0.1
    t = impulse KR x 0
    f = sweep t 700 + 500
in sinOsc AR f 0 * 0.2

-- sweep ; using sweep to index into a buffer ; requires=buf
let b = control KR "buf" 0
    x = mouseX KR 0.5 20 Exponential 0.1
    t = impulse AR x 0
    p = sweep t (bufSampleRate KR 0)
in bufRdL 1 AR 0 p NoLoop

-- sweep ; backwards, variable offset ; requires=buf
let n = lfNoise0 'α' KR 15
    x = mouseX KR 0.5 10 Exponential 0.1
    t = impulse AR x 0
    r = bufSampleRate KR 0
    p = sweep t (negate r) + (bufFrames KR 0 * n)
in bufRdL 1 AR 0 p NoLoop

-- sweep ; raising rate ; requires=buf
let x = mouseX KR 0.5 10 Exponential 0.1
    t = impulse AR x 0
    r = sweep t 2 + 0.5
    p = sweep t (bufSampleRate KR 0 * r)
in bufRdL 1 AR 0 p NoLoop

-- sweep ; f0 (sc-users, 2012-02-09)
let lf = range 0.01 1.25 (lfNoise2 'α' KR 1)
    du = duty AR lf 0 DoNothing lf
    tr = abs (hpz1 du) `greater_than` 0
    ph = sweep tr (1/du)
    fr = linExp ph 0 1 400 800
in sinOsc AR fr 0 * 0.2

-- sweep ; line segments, set start & end values, transition time and trigger, stops at end time
let tr = tr_control "tr" 1
    st = control KR "st" 440
    en = control KR "en" 880
    tm = control KR "tm" 2
    rt = ((en - st) / tm)
    sw = gate (sweep tr rt + st) (trig tr tm)
in sinOsc AR sw 0 * 0.2

---- ; send new line segments to running sweeper
withSC3 (Sound.OSC.sendMessage (n_set (-1) [("st",660),("en",550),("tm",4),("tr",1)]))
withSC3 (Sound.OSC.sendMessage (n_set (-1) [("st",110),("en",990),("tm",1),("tr",1)]))
