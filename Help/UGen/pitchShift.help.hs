-- pitchShift
let s = sinOsc ar 440 0 * 0.1
    r = mouseX kr 0.5 2.0 Linear 0.1
    d = mouseY kr 0.0 0.1 Linear 0.1
in pitchShift s 0.2 r d 0

-- pitchShift
let s = soundIn 0
    pd = mouseX kr 0.0 0.1 Linear 0.1
    td = mouseY kr 0.0 0.1 Linear 0.1
in pitchShift s 0.2 (mce2 1.0 1.5) pd td
