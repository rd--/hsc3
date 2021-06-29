-- perlin3
let x = integrator (k2a (mouseX kr 0 0.1 Linear 0.2)) 1.0
    y = integrator (k2a (mouseY kr 0 0.1 Linear 0.2)) 1.0
in X.perlin3 ar x y 0 * 0.1
