-- http://www.fredrikolofsson.com/f0blog/?q=node/617 (f0)
let c = 200000
    b = clearBuf (localBuf 2 c)
    d = bufRd 2 ar b (sinOsc ar (mce2 2 3 * 9) 0 * c) NoLoop LinearInterpolation
    w = bufWr b (abs (sinOsc ar (mce2 99 145) 0) * c) Loop (sinOsc ar (3 / mce2 2 3) 0 / 3)
in mrg2 (d * 0.1) w

-- http://www.fredrikolofsson.com/f0blog/?q=node/617 (f0) ; id
let c = 200000
    b = clearBuf (localBufId 'α' 2 c)
    d = bufRd 2 ar b (sinOsc ar (mce2 2 3 * 9) 0 * c) NoLoop LinearInterpolation
    w = bufWr b (abs (sinOsc ar (mce2 99 145) 0) * c) Loop (sinOsc ar (3 / mce2 2 3) 0 / 3)
in mrg2 (d * 0.1) w
