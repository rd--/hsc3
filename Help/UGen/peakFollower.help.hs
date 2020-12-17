-- peakFollower ; no decay
let d = dust 'α' AR 20 * line KR 0 1 4 DoNothing
in sinOsc AR (peakFollower d 1 * 1500 + 200) 0 * 0.1

-- peakFollower ; a little decay
let d = dust 'α' AR 20 * line KR 0 1 4 DoNothing
in sinOsc AR (peakFollower d 0.999 * 1500 + 200) 0 * 0.1

-- peakFollower ; mouse x controls decay
let dcy = mouseX KR 0.99 1.00001 Linear 0.2 `min` 1
    d = dust 'α' AR 20
in sinOsc AR (peakFollower d dcy * 1500 + 200) 0 * 0.1

-- peakFollower ; follow a sine lfo, decay controlled by mouse x
let dcy = mouseX KR 0 1.1 Linear 0.2 `min` 1
    sig = sinOsc KR 0.2 0
in sinOsc AR (mce2 sig (peakFollower sig dcy) * 200 + 500) 0 * 0.1
