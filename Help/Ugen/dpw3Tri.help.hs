-- dpw3Tri ; distortion creeps in under 200Hz
X.dpw3Tri ar (xLine kr 2000 20 10 DoNothing) * 0.1

-- dpw3Tri ; very fast sweeps can have transient distortion effects ; c.f. lfTri
X.dpw3Tri ar (mouseX kr 200 12000 Exponential 0.2) * 0.1

-- dpw3Tri ; less efficient than LFTri
let f = X.randNId 50 'α' 50 5000
in splay (X.dpw3Tri ar f) 1 0.1 0 True

-- dpw3Tri ; differentiation of triangle is square
let f = mouseX kr 440 8800 Exponential 0.2
    o = X.dpw3Tri ar f
in hpz1 (o * 2) * 0.25

-- dpw3Tri ; c.f. pulse
let f = mouseX kr 440 8800 Exponential 0.2
in mce2 (X.dpw3Tri ar f) (pulse ar f 0.5) * 0.05
