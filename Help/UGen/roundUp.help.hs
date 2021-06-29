-- roundUp
let x = mouseX kr 60 4000 Linear 0.1
    f = roundUp x 100
in sinOsc ar f 0 * 0.1

-- roundUp
let n = line kr 24 108 6 RemoveSynth
in saw ar (midiCPS (roundUp n 1)) * 0.2
