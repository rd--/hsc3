-- gbmanN ; default initial params
let x = mouseX KR 20 sampleRate Linear 0.2
in gbmanN AR x 1.2 2.1 * 0.05

-- gbmanN ; change initial params
let x = mouseX KR 20 sampleRate Linear 0.2
in gbmanN AR x (-0.7) (-2.7) * 0.05

-- gbmanN ; wait
let x = mouseX KR 20 sampleRate Linear 0.2
in gbmanN AR x 1.2 2.0002 * 0.05

-- gbmanN ; as a frequency control
let f = gbmanN AR 40 1.2 2.1 * 400 + 500
in sinOsc AR f 0 * 0.1
