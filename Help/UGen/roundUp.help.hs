-- roundUp
let x = mouseX KR 60 4000 Linear 0.1
    f = roundUp x 100
in sinOsc AR f 0 * 0.1

-- roundUp
let n = line KR 24 108 6 RemoveSynth
in saw AR (midiCPS (roundUp n 1)) * 0.2
