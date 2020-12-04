-- http://www.fredrikolofsson.com/f0blog/?q=node/617 (f0)
let c = 200000
    b = clearBuf (localBuf 'α' 2 c)
    d = bufRd 2 AR b (sinOsc AR (mce2 2 3 * 9) 0 * c) NoLoop LinearInterpolation
    w = bufWr b (abs (sinOsc AR (mce2 99 145) 0) * c) Loop (sinOsc AR (3 / mce2 2 3) 0 / 3)
in mrg2 (d * 0.1) w
