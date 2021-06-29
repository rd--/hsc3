-- peakFollower ; no decay
let d = dust 'α' ar 20 * line kr 0 1 4 DoNothing
in sinOsc ar (peakFollower d 1 * 1500 + 200) 0 * 0.1

-- peakFollower ; a little decay
let d = dust 'α' ar 20 * line kr 0 1 4 DoNothing
in sinOsc ar (peakFollower d 0.999 * 1500 + 200) 0 * 0.1

-- peakFollower ; mouse x controls decay
let dcy = mouseX kr 0.99 1.00001 Linear 0.2 `min` 1
    d = dust 'α' ar 20
in sinOsc ar (peakFollower d dcy * 1500 + 200) 0 * 0.1

-- peakFollower ; follow a sine lfo, decay controlled by mouse x
let dcy = mouseX kr 0 1.1 Linear 0.2 `min` 1
    sig = sinOsc kr 0.2 0
in sinOsc ar (mce2 sig (peakFollower sig dcy) * 200 + 500) 0 * 0.1
