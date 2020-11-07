-- rlpf
let n = whiteNoise 'α' AR
    f = sinOsc AR 0.5 0 * 40 + 220
in rlpf n f 0.1

-- rlpf
let f = fSinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
in rlpf (saw AR 200 * 0.1) f 0.2

-- rlpf
let ctl = rlpf (saw AR 5 * 0.1) 25 0.03
in sinOsc AR (ctl * 200 + 400) 0 * 0.1

-- rlpf ; mouse-control
let x = mouseX KR 2 200 Exponential 0.2
    y = mouseY KR 0.01 1 Exponential 0.2
    ctl = rlpf (saw AR 5 * 0.1) x y
in sinOsc AR (ctl * 200 + 400) 0 * 0.1

-- rlpf ; c.f. rlpfd
let s = mix (lfSaw AR (mce2 120 180) 0 * 0.33)
    f = linExp (lfCub KR 0.1 (0.5 * pi)) (-1) 1 280 1500
    rq = mouseX KR 0.05 0.5 Linear 0.2
in rlpf s f rq * 0.1
