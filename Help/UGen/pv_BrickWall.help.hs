-- pv_BrickWall
let z = soundIn 0
    x = mouseX KR (-1) 1 Linear 0.1
    c = fft' (localBuf 'α' 2048 1) z
in ifft' (pv_BrickWall c x)

-- pv_BrickWall ; ln 2021-04-12 https://lukasnowok.github.io/spectrology/
let c0 = localBuf 'α' 1 1024
    c1 = fft' c0 (whiteNoise 'β' AR)
    c2 = pv_BrickWall c1 (line AR (-1) 1 20 DoNothing)
in ifft' c2 * 0.1

-- pv_BrickWall ; ln 2021-04-18 https://lukasnowok.github.io/spectrology/
let e1 = envGen AR (impulse KR 1.1 0) 1 0 1 DoNothing (envPerc 0 1.2)
    o1 = ifft' (pv_BrickWall (fft' (localBuf 'α' 1 1024) (whiteNoise 'β' AR * e1)) 0.601)
    e2 = envGen AR (impulse KR 0.7 0) 1 0 1 DoNothing (envPerc 1 0)
    o2 = ifft' (pv_BrickWall (fft' (localBuf 'γ' 1 1024) (whiteNoise 'δ' AR * e2 * xLine AR 0.01 1 20 DoNothing)) (-0.5))
    o3 = lfTri AR 12000  0
in (o1 + o2 + o3) * 0.1

-- pv_BrickWall ; ln 2021-04-19 https://lukasnowok.github.io/spectrology/
let geom k z m = mce (take k (iterate (* m) z))
    s = mix (sinOsc AR (geom 100 20000 1.1 * line AR 1 0 20 DoNothing) 0 * 0.1)
    w = lfSaw AR (xLine AR 4 0.1 20 DoNothing) 0
in ifft' (pv_BrickWall (fft' (localBuf 'α' 1 1024) s) w) * 0.1

---- ; drawings
UI.ui_baudline 4096 50 "linear" 2
