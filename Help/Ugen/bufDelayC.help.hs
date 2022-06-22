-- bufDelayC ; dust randomly triggers decay to create an envelope
let b = localBufId 'α' 1 44100
    t = dustId 'β' ar 1
    n = whiteNoiseId 'γ' ar
    d = decay t 0.5 * n * 0.2
in bufDelayC b d 0.2 + d

-- bufDelayC ; mouse control for delay time
let b = localBufId 'α' 1 44100
    t = dustId 'β' ar 1
    n = whiteNoiseId 'γ' ar
    d = decay t 0.3 * n * 0.2
    x = mouseX kr 0.0 0.2 Linear 0.1
in d + bufDelayC b d x

-- bufDelayC
let b = localBufId 'α' 1 44100
    o = sinOsc ar (lfNoise2Id 'β' kr 0.5 * 100 + 110) 0 * 0.05
    d = abs (lfNoise2Id 'γ' kr 0.25)
in mce2 o (bufDelayC b o d)
