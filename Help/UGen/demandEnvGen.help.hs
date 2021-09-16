-- demandEnvGen ; frequency ramp, exponential curve
let l = dseqId 'α' dinf (mce2 440 9600)
    y = mouseY kr 0.01 3 Exponential 0.2
    s = env_curve_shape EnvExp
    f = demandEnvGen ar l y s 0 1 1 1 0 1 DoNothing
in sinOsc ar f 0 * 0.1

-- demandEnvGen ; frequency envelope with random times
let l = dseqId 'α' dinf (mce [204,400,201,502,300,200])
    t = drandId 'β' dinf (mce [1.01,0.2,0.1,2.0])
    y = mouseY kr 0.01 3 Exponential 0.2
    s = env_curve_shape EnvCub
    f = demandEnvGen ar l (t * y) s 0 1 1 1 0 1 DoNothing
in sinOsc ar (f * mce2 1 1.01) 0 * 0.1

-- demandEnvGen ; Frequency modulation
let n = dwhiteId 'α' dinf 200 1000
    x = mouseX kr (-0.01) (-4) Linear 0.2
    y = mouseY kr 1 3000 Exponential 0.2
    s = env_curve_shape (EnvNum undefined)
    f = demandEnvGen ar n (sampleDur * y) s x 1 1 1 0 1 DoNothing
in sinOsc ar f 0 * 0.1

-- demandEnvGen ; Short sequence with doneAction, linear
let l = dseqId 'α' 1 (mce [1300,500,800,300,400])
    s = env_curve_shape EnvLin
    f = demandEnvGen kr l 2 s 0 1 1 1 0 1 RemoveSynth
in sinOsc ar (f * mce2 1 1.01) 0 * 0.1

-- demandEnvGen ; Gate, mouse x on right side of screen toggles gate
let n = roundTo (dwhiteId 'α' dinf 300 1000) 100
    x = mouseX kr 0 1 Linear 0.2
    g = x >** 0.5
    f = demandEnvGen ar n 0.1 5 0.3 g 1 1 0 1 DoNothing
in sinOsc ar (f * mce2 1 1.21) 0 * 0.1

-- demandEnvGen ; gate, mouse x toggles sample and hold, mouse button does hard reset
let l = dseqId 'α' 2 (mce [dseriesId 'β' 5 400 200,500,800,530,4000,900])
    x = mouseX kr 0 1 Linear 0.2
    g = (x >** 0.5) - 0.1
    b = mouseButton kr 0 1 0.2
    r = (b >** 0.5) * 2
    s = env_curve_shape EnvSin
    f = demandEnvGen kr l 0.1 s 0 g r 1 0 1 DoNothing
in sinOsc ar (f * mce2 1 1.001) 0 * 0.1

-- demandEnvGen ; coordinate buffer, layout is (initial-level,duration,level,..,loop-duration)
let b = asLocalBufId 'α' [0,0.5,0.1,0.5,1,0.01]
    l_i = dseriesId 'β' dinf 0 2
    d_i = dseriesId 'γ' dinf 1 2
    l = dbufrdId 'δ' b l_i Loop
    d = dbufrdId 'ε' b d_i Loop
    s = env_curve_shape EnvLin
    e = demandEnvGen kr l d s 0 1 1 1 0 5 RemoveSynth
    f = midiCps (60 + (e * 12))
in sinOsc ar (f * mce2 1 1.01) 0 * 0.1

-- demandEnvGen ; read envelope break-points from buffer, here simply duration/level pairs.
--              ; the behavior is odd if the curve is zero (ie. flat segments).
let b = asLocalBufId 'α' [61,1,60,2,72,1,55,5,67,9,67]
    lvl = dbufrdId 'β' b (dseriesId 'γ' 6 0 2) Loop
    dur = dbufrdId 'δ' b (dseriesId 'ε' 5 1 2) Loop
    e = demandEnvGen kr lvl dur 1 0 1 1 1 0 1 RemoveSynth
in sinOsc ar (midiCps e) 0 * 0.1

-- demandEnvGen ; lfNoise1
let y = mouseY kr 0.5 20 Linear 0.2
    lvl = dwhiteId 'β' dinf (-0.1) 0.1
    dur = sampleDur * y
in demandEnvGen ar lvl dur 5 (-4) 1 1 1 0 1 RemoveSynth

-- demandEnvGen ; lfBrownNoise
let y = mouseY kr 1 100 Exponential 0.2
    lvl = dbrownId 'β' dinf (-0.1) 0.1 0.1
    dur = sampleDur * y
in demandEnvGen ar lvl dur 1 0 1 1 1 0 1 RemoveSynth
