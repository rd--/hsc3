-- playBufCF ; wslib ; CF = cross-fade ; control-rate trigger and start-position inputs
let b = control kr "buf" 0
    r = bufRateScale kr b
    tr = impulse kr 2 0
    wn = whiteNoiseId 'α' kr
    sp = linLin wn (-1) 1 0 (bufFrames kr b - (0.5 * 44100))
    o = playBufCF 1 b r tr sp NoLoop 0.1 2
    o' = playBuf 1 ar b r tr sp NoLoop DoNothing
in mce2 o o'

-- playBufCF ; demand ugens inputs
let b = control kr "buf" 0
    r = drandId 'α' dinf (mce [0.95,1,1.05])
    tr = dwhiteId 'β' dinf 0.1 0.3
    sp = dbrownId 'γ' dinf 0 0.95 0.1 * bufFrames kr b
in playBufCF 1 b r tr sp NoLoop 2 5

---- ; load sound file to buffer zero (single channel file required for examples)
withSC3 (async (b_allocRead 0 (sfRequire "pf-c5.aif") 0 0))
withSC3 (async (b_allocRead 0 (sfRequire "a11wlk01.wav") 0 0))
