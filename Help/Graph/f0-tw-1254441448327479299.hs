-- f0 <https://twitter.com/redFrik/status/1254441448327479299>
let b = [1,3,5,8,10]
    e = [3,2/3,4,3/2,2]
    c = 0.021
    d = lfTri AR (mce b / 999) 0 `modE` 1
    m = lfTri AR (mce b * c) 0
    l = m * 7 + 20 + dseq 'α' dinf (mce b `modE` m * 5 + 6)
    j = duty AR (mce e / (12 ** m)) 0 DoNothing l
    f = midiCPS (degreeToKey (asLocalBuf 'β' b) j 12)
    y = decay2 (impulse AR (mce [2/3,1.5,3,1.5,3]) 0) c d * d
    o = sinOscFB AR f ((lfTri AR (c / mce b) 0 + 1) / 3) * y
in freeVerb (splay o 1 1 0 True) 0.1 1 0.5 * 0.2

-- rd (edit) ; f0 <https://twitter.com/redFrik/status/1254441448327479299>
let b = [1,3,5,8,10]
    e = [3,2/3,4,3/2,2]
    c = 0.021
    d = lfTri AR (mce b / 999) 0 `modE` 1
    m = lfTri AR (mce b * c) 0
    l = m * 7 + 20 + dseq 'α' dinf (mce b `modE` m * 5 + 6)
    j = duty AR (mce e / (12 ** m)) 0 DoNothing l
    k = degreeToKey (asLocalBuf 'β' b) j 12
    o = sinOscFB AR (midiCPS k) (lfTri AR (((c / mce b) + 1) / 3) 1 * d)
    r = freeVerb (splay o 1 1 0 True) 0.1 1 0.5 * 0.2
    p = out 90 (a2k k)
in mrg2 r p

---- ; drawings
UI.ui_sc3_scope 2 0 (2 ^ 14) 0 "audio" 0
UI.ui_sc3_ctl_plot 1200 5 90 1200 (1/25) 10
