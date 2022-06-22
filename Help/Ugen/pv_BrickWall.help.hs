-- pv_BrickWall
let z = soundIn 0
    x = mouseX kr (-1) 1 Linear 0.1
    c = fft' (localBufId 'α' 2048 1) z
in ifft' (pv_BrickWall c x)

-- pv_BrickWall ; ln 2021-04-12 https://lukasnowok.github.io/spectrology/
let c0 = localBufId 'α' 1 1024
    c1 = fft' c0 (whiteNoiseId 'β' ar)
    c2 = pv_BrickWall c1 (line ar (-1) 1 20 DoNothing)
in ifft' c2 * 0.1

-- pv_BrickWall ; ln 2021-04-18 https://lukasnowok.github.io/spectrology/
let e1 = envGen ar (impulse kr 1.1 0) 1 0 1 DoNothing (envPerc 0 1.2)
    o1 = ifft' (pv_BrickWall (fft' (localBufId 'α' 1 1024) (whiteNoiseId 'β' ar * e1)) 0.601)
    e2 = envGen ar (impulse kr 0.7 0) 1 0 1 DoNothing (envPerc 1 0)
    o2 = ifft' (pv_BrickWall (fft' (localBufId 'γ' 1 1024) (whiteNoiseId 'δ' ar * e2 * xLine ar 0.01 1 20 DoNothing)) (-0.5))
    o3 = lfTri ar 12000  0
in (o1 + o2 + o3) * 0.1

-- pv_BrickWall ; ln 2021-04-19 https://lukasnowok.github.io/spectrology/
let geom k z m = mce (take k (iterate (* m) z))
    s = mix (sinOsc ar (geom 100 20000 1.1 * line ar 1 0 20 DoNothing) 0 * 0.1)
    w = lfSaw ar (xLine ar 4 0.1 20 DoNothing) 0
in ifft' (pv_BrickWall (fft' (localBufId 'α' 1 1024) s) w) * 0.1

---- ; drawings
UI.ui_baudline 4096 50 "linear" 2
