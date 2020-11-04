-- demandEnvGen ; frequency ramp, exponential curve
let l = dseq 'α' dinf (mce2 440 9600)
    y = mouseY KR 0.01 3 Exponential 0.2
    s = env_curve_shape EnvExp
    f = demandEnvGen AR l y s 0 1 1 1 0 1 DoNothing
in sinOsc AR f 0 * 0.1

-- demandEnvGen ; frequency envelope with random times
let l = dseq 'α' dinf (mce [204,400,201,502,300,200])
    t = drand 'β' dinf (mce [1.01,0.2,0.1,2.0])
    y = mouseY KR 0.01 3 Exponential 0.2
    s = env_curve_shape EnvCub
    f = demandEnvGen AR l (t * y) s 0 1 1 1 0 1 DoNothing
in sinOsc AR (f * mce2 1 1.01) 0 * 0.1

-- demandEnvGen ; Frequency modulation
let n = dwhite 'α' dinf 200 1000
    x = mouseX KR (-0.01) (-4) Linear 0.2
    y = mouseY KR 1 3000 Exponential 0.2
    s = env_curve_shape (EnvNum undefined)
    f = demandEnvGen AR n (sampleDur * y) s x 1 1 1 0 1 DoNothing
in sinOsc AR f 0 * 0.1

-- demandEnvGen ; Short sequence with doneAction, linear
let l = dseq 'α' 1 (mce [1300,500,800,300,400])
    s = env_curve_shape EnvLin
    f = demandEnvGen KR l 2 s 0 1 1 1 0 1 RemoveSynth
in sinOsc AR (f * mce2 1 1.01) 0 * 0.1

-- demandEnvGen ; Gate, mouse x on right side of screen toggles gate
let n = roundTo (dwhite 'α' dinf 300 1000) 100
    x = mouseX KR 0 1 Linear 0.2
    g = x >** 0.5
    f = demandEnvGen AR n 0.1 5 0.3 g 1 1 0 1 DoNothing
in sinOsc AR (f * mce2 1 1.21) 0 * 0.1

-- demandEnvGen ; gate, mouse x toggles sample and hold, mouse button does hard reset
let l = dseq 'α' 2 (mce [dseries 'β' 5 400 200,500,800,530,4000,900])
    x = mouseX KR 0 1 Linear 0.2
    g = (x >** 0.5) - 0.1
    b = mouseButton KR 0 1 0.2
    r = (b >** 0.5) * 2
    s = env_curve_shape EnvSin
    f = demandEnvGen KR l 0.1 s 0 g r 1 0 1 DoNothing
in sinOsc AR (f * mce2 1 1.001) 0 * 0.1

-- demandEnvGen ; coordinate buffer, layout is (initial-level,duration,level,..,loop-duration)
let b = asLocalBuf 'α' [0,0.5,0.1,0.5,1,0.01]
    l_i = dseries 'β' dinf 0 2
    d_i = dseries 'γ' dinf 1 2
    l = dbufrd 'δ' b l_i Loop
    d = dbufrd 'ε' b d_i Loop
    s = env_curve_shape EnvLin
    e = demandEnvGen KR l d s 0 1 1 1 0 5 RemoveSynth
    f = midiCPS (60 + (e * 12))
in sinOsc AR (f * mce2 1 1.01) 0 * 0.1

-- demandEnvGen ; read envelope break-points from buffer, here simply duration/level pairs.
--              ; the behavior is odd if the curve is zero (ie. flat segments).
let b = asLocalBuf 'α' [61,1,60,2,72,1,55,5,67,9,67]
    lvl = dbufrd 'β' b (dseries 'γ' 6 0 2) Loop
    dur = dbufrd 'δ' b (dseries 'ε' 5 1 2) Loop
    e = demandEnvGen KR lvl dur 1 0 1 1 1 0 1 RemoveSynth
in sinOsc AR (midiCPS e) 0 * 0.1

-- demandEnvGen ; lfNoise1
let y = mouseY KR 0.5 20 Linear 0.2
    lvl = dwhite 'β' dinf (-0.1) 0.1
    dur = sampleDur * y
in demandEnvGen AR lvl dur 5 (-4) 1 1 1 0 1 RemoveSynth

-- demandEnvGen ; lfBrownNoise
let y = mouseY KR 1 100 Exponential 0.2
    lvl = dbrown 'β' dinf (-0.1) 0.1 0.1
    dur = sampleDur * y
in demandEnvGen AR lvl dur 1 0 1 1 1 0 1 RemoveSynth
