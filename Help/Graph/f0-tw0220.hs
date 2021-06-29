-- http://www.fredrikolofsson.com/f0blog/?q=node/617 (f0)
let c = inFeedback 1 0
    b = clearBuf (localBuf 1 90000)
    g = tGrains 2 (sinOsc ar 3 0) b (c + 3) 2 12 0 0.1 4
    r = recordBuf ar b 0 1 0 1 Loop 1 DoNothing c
in mrg2 (hpf (sinOsc ar 99 (c * 6) / 9 + g) 9 * 0.75) r

-- http://www.fredrikolofsson.com/f0blog/?q=node/617 (f0) ; id
let c = inFeedback 1 0
    b = clearBuf (localBufId 'α' 1 90000)
    g = tGrains 2 (sinOsc ar 3 0) b (c + 3) 2 12 0 0.1 4
    r = recordBuf ar b 0 1 0 1 Loop 1 DoNothing c
in mrg2 (hpf (sinOsc ar 99 (c * 6) / 9 + g) 9 * 0.75) r
