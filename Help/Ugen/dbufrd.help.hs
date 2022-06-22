-- dbufrd ; pattern as frequency input
let n = System.Random.randomRs (200.0,500.0) (System.Random.mkStdGen 0)
    b = asLocalBufId 'α' (take 24 n)
    s = dseqId 'β' 3 (mce [0,3,5,0,3,7,0,5,9])
    p = dseqId 'γ' dinf (mce [s,dbrownId 'δ' 5 0 23 1])
    t = dustId 'ε' kr 10
    r = dbufrdId 'ζ' b p Loop
in sinOsc ar (demand t 0 r) 0 * 0.1

-- dbufrd ; time pattern
let n1 = System.Random.randomRs (200.0,500.0) (System.Random.mkStdGen 0)
    b1 = asLocalBufId 'α' (take 24 n1)
    n2 = map ([1,0.5,0.25] !!) (System.Random.randomRs (0,2) (System.Random.mkStdGen 1))
    b2 = asLocalBufId 'β' (take 24 n2)
    s = dseqId 'γ' 3 (mce [0,3,5,0,3,7,0,5,9])
    p = dseqId 'δ' dinf (mce [s,dbrownId 'ε' 5 0 23 1])
    j = dseriesId 'ζ' dinf 0 1
    d = dbufrdId 'η' b2 j Loop
    l = dbufrdId 'θ' b1 p Loop
    f = duty kr (d * 0.5) 0 DoNothing l
in sinOsc ar f 0 * 0.1
