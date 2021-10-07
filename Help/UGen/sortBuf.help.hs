-- sortBuf ; gradual erosion
let bufnum = 0
in pan2 (X.sortBuf ar bufnum (lfNoise0 kr 5 * 50000 + 60000) 0) 0 0.1

---- ; load sndfile
withSC3 (async (b_allocRead 0 (sfRequire "pf-c5.aif") 0 0))
withSC3 (async (b_allocRead 0 (sfRequire "a11wlk01.wav") 0 0))
