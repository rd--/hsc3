-- detectSilence
let s = sinOsc ar 440 0 * mouseY kr 0 0.2 Linear 0.1
    d = detectSilence s 0.01 0.1 RemoveSynth
in mrg [s,d]
