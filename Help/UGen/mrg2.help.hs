-- mrg2 ; defines a node indicating a multiple root graph.
let l = out 0 (sinOsc AR 300 0 * 0.1)
    r = out 1 (sinOsc AR 900 0 * 0.1)
in mrg2 l r

-- mrg2 ; there is a leftmost rule, so that mrg nodes need not be terminal
let l = sinOsc AR 300 0 * 0.1
    r = out 1 (sinOsc AR 900 0 * 0.1)
in mrg2 l r

-- mrg2 ; the leftmost node may be an mce node
let l = sinOsc AR (mce2 300 400) 0 * 0.1
    r = out 1 (sinOsc AR 900 0 * 0.1)
in mrg2 l r

-- mrg2 ; the implementation is not thorough
let l = sinOsc AR (mce2 300 400) 0 * 0.1
    r = out 1 (sinOsc AR 900 0 * 0.1)
in out 0 (mrg2 l r + mrg2 l r)
