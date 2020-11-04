-- blitB3Tri
X.blitB3Tri AR (xLine KR 1000 20 10 DoNothing) 0.99 0.99 * 0.1

-- blitB3Tri ; aliasing higher frequencies (over 5000Hz or so) is very beautiful in point scope
let x = mouseX KR 20 8000 Exponential 0.2
    y = mouseY KR 0.001 0.99 Linear 0.2
in X.blitB3Tri AR x 0.99 y * 0.1

-- c.f. lfTri ; more efficient, some aliasing from 3000hz ; less harmonics for lower fundamentals
let x = mouseX KR 20 8000 Exponential 0.2
in lfTri AR x 0 * 0.1
