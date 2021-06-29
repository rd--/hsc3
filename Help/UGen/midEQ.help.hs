-- midEQ
let f = midiCPS (fSinOsc kr 1 0 * 24 + 84)
in midEQ (saw ar 200 * 0.05) f 0.3 12

-- midEQ
let i = pinkNoise 'α' ar * 0.1 + sinOsc ar 600 0 * 0.05
    f = sinOsc kr 0.2 (0.5 * pi) * 2 + 600
in midEQ i f 0.01 (-24)
