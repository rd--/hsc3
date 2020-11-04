-- dpw4Saw
X.dpw4Saw AR (xLine KR 2000 20 10 DoNothing) * 0.1

-- dpw4Saw ; c.f. saw
let x = mouseX KR 200 12000 Exponential 0.2
in mce2 (X.dpw4Saw AR x * 0.1) (saw AR x * 0.05)
