-- slope
let a = lfNoise2 'α' ar 2 {- quadratic noise -}
    b = slope a {- first derivative, line segments -}
    c = slope b {- second derivative, constant segments -}
    s = 0.0002
    f = mce [a, b * s, c * s * s] * 220 + 220
in mix (sinOsc ar f 0 * 0.1)

---- ; drawings
let {a = lfNoise2 'α' ar 2000;b = slope a;c = slope b;m = 0.0002}
Sound.SC3.Plot.plot_ugen 0.05 (mce [a,b * m,c * m * m])
