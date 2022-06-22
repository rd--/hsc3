-- gbmanN ; default initial params
let x = mouseX kr 20 sampleRate Linear 0.2
in gbmanN ar x 1.2 2.1 * 0.05

-- gbmanN ; change initial params
let x = mouseX kr 20 sampleRate Linear 0.2
in gbmanN ar x (-0.7) (-2.7) * 0.05

-- gbmanN ; wait
let x = mouseX kr 20 sampleRate Linear 0.2
in gbmanN ar x 1.2 2.0002 * 0.05

-- gbmanN ; as a frequency control
let f = gbmanN ar 40 1.2 2.1 * 400 + 500
in sinOsc ar f 0 * 0.1
