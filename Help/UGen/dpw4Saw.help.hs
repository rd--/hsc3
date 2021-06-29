-- dpw4Saw
X.dpw4Saw ar (xLine kr 2000 20 10 DoNothing) * 0.1

-- dpw4Saw ; c.f. saw
let x = mouseX kr 200 12000 Exponential 0.2
in mce2 (X.dpw4Saw ar x * 0.1) (saw ar x * 0.05)
