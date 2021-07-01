-- tap
let b = clearBuf (localBufId 'α' 1 48000) -- sample-rate
    src = soundIn 0 -- use headphones
    put = bufWr b (phasor ar 0 1 0 (bufFrames kr b) 1) Loop src
    get = tap 1 ar b (mce2 0.1 0.9)
in mrg2 get put
