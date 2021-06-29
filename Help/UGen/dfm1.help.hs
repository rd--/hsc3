-- dfm1 ; mouse control
let n = pinkNoise 'α' ar * 0.5
    x = mouseX kr 80 5000 Exponential 0.1
    y = mouseY kr 0.1 1.2 Linear 0.1
in X.dfm1 n x y 1 0 3e-4

-- dfm1 ; bass
let i = mix (pulse ar (mce2 100 100.1) 0.5) * 0.4
    f = range 80 2000 (sinOsc kr (range 0.2 5 (sinOsc kr 0.3 0)) 0)
in X.dfm1 i f 1.1 2 0 0.0003 * 0.1
