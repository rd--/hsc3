-- blitB3
X.blitB3 ar (xLine kr 10000 20 10 DoNothing) * 0.2

-- blitB3 ; spot the aliasing
impulse ar (xLine kr 10000 20 10 DoNothing) 0 * 0.2

-- blitB3 ; sawtooth
let x = mouseX kr 20 1000 Exponential 0.2
in leakDC (integrator (X.blitB3 ar x * 0.2) 0.99) 0.995

-- blitB3 ; sawtooth, super-saw, can integrate, accumulates DC
let x = mouseX kr 1 4 Linear 0.2
in mix (leakDC (integrator (X.blitB3 ar (x * mce [220,221,223,224]) * 0.125) 0.99) 0.995)
