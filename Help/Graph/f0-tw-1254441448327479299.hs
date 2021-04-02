-- https://twitter.com/redFrik/status/1254441448327479299
let b = [1,3,5,8,10]
    e = mce [3,2/3,4,3/2,2]
    a = lfTri AR
    c = 0.021
    d = a (mce b / 999) 0 `modE` 1
    j = duty AR (e/(12 ** a (mce b * c) 0)) 0 DoNothing (a (mce b * c) 0 * 7 + 20 + dseq 'α' dinf (mce b `modE` a (mce b * c) 0 * 5 + 6))
    f = midiCPS (degreeToKey (asLocalBuf 'β' b) j 12)
    o = sinOscFB AR f (a (((c / mce b)+1)/3) (decay2 (impulse AR (mce [2/3,1.5,3,1.5,3]) 0) c d) * d)
in freeVerb (splay o 1 1 0 True) 0.1 1 0.5 * 0.2

-- rd (edit) / f0 <https://twitter.com/redFrik/status/1254441448327479299>
let b = [1,3,5,8,10]
    e = mce [3,2/3,4,3/2,2]
    a = lfTri AR
    c = 0.021
    d = a (mce b / 999) 0 `modE` 1
    j = duty AR (e / (12 ** a (mce b * c) 0)) 0 DoNothing (a (mce b * c) 0 * 7 + 20 + dseq 'α' dinf (mce b `modE` a (mce b * c) 0 * 5 + 6))
    f = midiCPS (degreeToKey (asLocalBuf 'β' b) j 12)
    o = sinOscFB AR f (a (((c / mce b) + 1) / 3) (decay2 (impulse AR (mce [2/3,1.5,3,1.5,3]) 0) c d) * d)
in freeVerb (splay o 1 1 0 True) 0.1 1 0.5 * 0.2
