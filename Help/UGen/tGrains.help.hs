-- tGrains ; mouse control
let b = control KR "buf" 0
    tRate = mouseY KR 2 200 Exponential 0.1
    ctr = mouseX KR 0 (bufDur KR 0) Linear 0.1
    tr = impulse AR tRate 0
in tGrains 2 tr b 1 ctr (4 / tRate) 0 0.25 2

-- tGrains ; mouse control
let b = control KR "buf" 0
    rt = mouseY KR 8 120 Exponential 0.1
    dur = 4 / rt
    clk = dust 'α' AR rt
    r = tRand 'β' 0 0.01 clk
    pan = whiteNoise 'γ' KR * 0.6
    x = mouseX KR 0 (bufDur KR b) Linear 0.1
    pos = x + r
in tGrains 2 clk b 1 pos dur pan 0.25 2

-- tGrains ; mouse control
let b = control KR "buf" 0
    rt = mouseY KR 2 120 Exponential 0.1
    dur = 1.2 / rt
    clk = impulse AR rt 0
    pos = mouseX KR 0 (bufDur KR b) Linear 0.1
    n0 = whiteNoise 'α' KR
    n1 = whiteNoise 'β' KR
    rate = shiftLeft 1.2 (roundTo (n0 * 3) 1)
in tGrains 2 clk b rate pos dur (n1 * 0.6) 0.25 2

-- tGrains ; demand UGens as inputs
--         ; (Warning: empty sequence in Dseq - Server 'localhost' exited with exit code 0)
--         ; (also at sclang graph)
let b = control KR "buf" 0
    rt = mouseX KR 1 100 Exponential 0.2
    d uid = dwhite uid 1 0.1 0.2
    z u0 u1 u2 u3 u4 u5 u6 u7 = drand u1 1 (mce [dgeom u2 (diwhite u3 1 20 40) 0.1 (1 + d u4)
                                                ,dgeom u5 (diwhite u6 1 20 40) 1 (1 - d u7)])
    clk = impulse AR rt 0
    dsq e xs = dseq e dinf (mce xs)
    rate = dsq 'α' [1,1,z 'β' 'γ' 'δ' 'ε' 'ζ' 'η' 'θ' 'ι',0.5,0.5,0.2,0.1,0.1,0.1,0.1] * 2 + 1
    pos = dsq 'κ' (Protect.uclone_seq (const False) 'λ' 8 (z 'μ' 'ν' 'ξ' 'ο' 'π' 'ρ' 'σ' 'τ'))
    dur = dsq 'υ' [1,d 'φ',1,z 'χ' 'ψ' 'ω' 'Α' 'Β' 'Γ' 'Δ' 'Ε',0.5,0.5,0.1,z 'Ζ' 'Η' 'Θ' 'Ι' 'Κ' 'Λ' 'Μ' 'Ν'] * 2 / rt
    pan = dsq 'Ξ' [1,1,1,0.5,0.2,0.1,0,0,0] * 2 - 1
    amp = dsq 'Ο' [1,0,z 'Π' 'Ρ' 'Σ' 'Τ' 'Υ' 'Φ' 'Χ' 'Ψ',0,2,1,1,0.1,0.1]
in tGrains 2 clk b rate pos dur pan amp 2

-- tGrains ; http://sc-users.bham.ac.narkive.com/sj4Tw3ub/sync-osc#post5 (jmcc)
--         ; "A wavetable windowed sync oscillator could be written.
--         ;  You can actually do a version of this with TGrains."
let b = control KR "buf" 0
    freq = 100
    dur = 2 / freq
    clk = impulse AR freq 0
    x = mouseX KR 0.5 16 Exponential 0.2
in tGrains 2 clk b x (0.3 * bufDur KR b) dur 0 0.1 2

---- ; setup
withSC3 (async (b_allocRead 0 "/home/rohan/data/audio/metal.wav" 0 0))
