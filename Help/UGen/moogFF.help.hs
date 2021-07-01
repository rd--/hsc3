-- moogFF
let n = whiteNoiseId 'α' ar * 0.05
    freq = mouseX kr 100 10000 Exponential 0.1
    gain = mouseY kr 0 4 Linear 0.1
in moogFF n freq gain 0

-- moogFF ; note distortion at high gain
let x = mouseX kr 100 20000 Exponential 0.1
    y = mouseY kr 0.1 4.0 Linear 0.1
    i = mix (saw ar (mce [0.99, 1, 1.01] * 440)) * 0.2
in moogFF i x y 0

-- moogFF
let n = lfNoise0Id 'α' kr 0.43
    p = pulse ar (mce [40, 121]) (mce [0.3, 0.7])
    f0 = linLin n 0 1 0.001 2.2
    f = linLin (sinOsc kr f0 0) (-1) 1 30 4200
    y = mouseY kr 1 4 Linear 0.1
in moogFF p f (0.83 * y) 0

---- ; drawings
UI.ui_sc3_scope_freq (600,400) 0
