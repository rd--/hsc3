-- tap
let b = clearBuf (localBuf 'α' 1 48000) -- sample-rate
    src = soundIn 0 -- use headphones
    put = bufWr b (phasor AR 0 1 0 (bufFrames KR b) 1) Loop src
    get = tap 1 AR b (mce2 0.1 0.9)
in mrg2 get put
