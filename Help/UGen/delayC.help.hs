-- delayC ; c.f. delayN ; no zipper noise
let i = sinOsc AR 320 0 * 0.1
    maxdelaytime = 0.005
    delaytime = mouseX KR 0.0 maxdelaytime Linear 0.15
in i + delayC i maxdelaytime delaytime
