-- detectSilence
let s = sinOsc AR 440 0 * mouseY KR 0 0.2 Linear 0.1
    d = detectSilence s 0.01 0.1 RemoveSynth
in mrg [s,d]
