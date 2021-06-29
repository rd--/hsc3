-- tGrains ; mouse control ; requires=buf
let b = control kr "buf" 0
    tRate = mouseY kr 2 200 Exponential 0.1
    ctr = mouseX kr 0 (bufDur kr b) Linear 0.1
    tr = impulse ar tRate 0
in tGrains 2 tr b 1 ctr (4 / tRate) 0 0.25 2

-- tGrains ; mouse control ; requires=buf
let b = control kr "buf" 0
    rt = mouseY kr 8 120 Exponential 0.1
    dur = 4 / rt
    clk = dust 'α' ar rt
    r = tRand 'β' 0 0.01 clk
    pan = whiteNoise 'γ' kr * 0.6
    x = mouseX kr 0 (bufDur kr b) Linear 0.1
    pos = x + r
in tGrains 2 clk b 1 pos dur pan 0.25 2

-- tGrains ; mouse control ; requires=buf
let b = control kr "buf" 0
    rt = mouseY kr 2 120 Exponential 0.1
    dur = 1.2 / rt
    clk = impulse ar rt 0
    pos = mouseX kr 0 (bufDur kr b) Linear 0.1
    n0 = whiteNoise 'α' kr
    n1 = whiteNoise 'β' kr
    rate = shiftLeft 1.2 (roundTo (n0 * 3) 1)
in tGrains 2 clk b rate pos dur (n1 * 0.6) 0.25 2

-- tGrains ; demand UGens as inputs ; requires=buf
--         ; (Warning: empty sequence in Dseq - Server 'localhost' exited with exit code 0)
--         ; (also at sclang graph)
let b = control kr "buf" 0
    rt = mouseX kr 1 100 Exponential 0.2
    d uid = dwhite uid 1 0.1 0.2
    z u0 u1 u2 u3 u4 u5 u6 u7 = drand u1 1 (mce [dgeom u2 (diwhite u3 1 20 40) 0.1 (1 + d u4)
                                                ,dgeom u5 (diwhite u6 1 20 40) 1 (1 - d u7)])
    clk = impulse ar rt 0
    dsq e xs = dseq e dinf (mce xs)
    rate = dsq 'α' [1,1,z 'β' 'γ' 'δ' 'ε' 'ζ' 'η' 'θ' 'ι',0.5,0.5,0.2,0.1,0.1,0.1,0.1] * 2 + 1
    pos = dsq 'κ' (Protect.uclone_seq (const False) 'λ' 8 (z 'μ' 'ν' 'ξ' 'ο' 'π' 'ρ' 'σ' 'τ'))
    dur = dsq 'υ' [1,d 'φ',1,z 'χ' 'ψ' 'ω' 'Α' 'Β' 'Γ' 'Δ' 'Ε',0.5,0.5,0.1,z 'Ζ' 'Η' 'Θ' 'Ι' 'Κ' 'Λ' 'Μ' 'Ν'] * 2 / rt
    pan = dsq 'Ξ' [1,1,1,0.5,0.2,0.1,0,0,0] * 2 - 1
    amp = dsq 'Ο' [1,0,z 'Π' 'Ρ' 'Σ' 'Τ' 'Υ' 'Φ' 'Χ' 'Ψ',0,2,1,1,0.1,0.1]
in tGrains 2 clk b rate pos dur pan amp 2

-- tGrains ; sync oscillator ; requires=buf
--         ; http://sc-users.bham.ac.narkive.com/sj4Tw3ub/sync-osc#post5 (jmcc)
--         ; "A wavetable windowed sync oscillator could be written.
--         ;  You can actually do a version of this with TGrains."
let b = control kr "buf" 0
    freq = 100
    dur = 2 / freq
    clk = impulse ar freq 0
    x = mouseX kr 0.5 16 Exponential 0.2
in tGrains 2 clk b x (0.3 * bufDur kr b) dur 0 0.1 2

-- tGrains ; requires=buf ; event control
let f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let b = control kr "buf" 0
          tRate = linExp y 0 1 2 200
          ctr = x * bufDur kr b
          du = (ry * 8) / tRate
          tr = impulse ar (y * 60 + 10) 0
      in tGrains 2 tr b (1 + (rx * 0.5)) ctr du o (z * g) 4
in mix (eventVoicer 16 f) * control kr "gain" 2

---- ; setup
fn = "/home/rohan/data/audio/metal.wav"
fn = "/home/rohan/data/audio/instr/celeste/long/37-C6-long.wav"
withSC3 (async (b_allocRead 0 fn 0 0))
