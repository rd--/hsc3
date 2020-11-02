-- blitB3Saw
let f = xLine KR 1000 20 10 DoNothing
in X.blitB3Saw AR f 0.99 * 0.1

-- blitB3Saw ; aliasing suddenly appears for very high frequencies
let f = mouseX KR 10 10000 Exponential 0.2
    c = mouseY KR 0.01 0.99 Linear 0.2
in X.blitB3Saw AR f c * 0.1

-- blitB3Saw ; comparison
mce2 (saw AR 20) (X.blitB3Saw AR 20 0.99) * 0.1
