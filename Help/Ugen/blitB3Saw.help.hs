-- blitB3Saw
let f = xLine kr 1000 20 10 DoNothing
in X.blitB3Saw ar f 0.99 * 0.1

-- blitB3Saw ; aliasing suddenly appears for very high frequencies
let f = mouseX kr 10 10000 Exponential 0.2
    c = mouseY kr 0.01 0.99 Linear 0.2
in X.blitB3Saw ar f c * 0.1

-- blitB3Saw ; comparison
mce2 (saw ar 20) (X.blitB3Saw ar 20 0.99) * 0.1
