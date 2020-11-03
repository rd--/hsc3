-- perlin3
let x = integrator (k2a (mouseX KR 0 0.1 Linear 0.2)) 1.0
    y = integrator (k2a (mouseY KR 0 0.1 Linear 0.2)) 1.0
in X.perlin3 AR x y 0 * 0.1
