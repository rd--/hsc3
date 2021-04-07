-- t2a
let tr = impulse KR (mouseX KR 1 100 Exponential 0.2) 0
in ringz (t2a tr 0) 800 0.01 * 0.4

-- t2a ; compare with k2a (oscilloscope)
let tr = impulse KR 200 0
in lag (mce2 (t2a tr 0) (k2a tr)) 0.001

-- t2a ; removing jitter by randomising offset
let tr = impulse KR (mouseX KR 1 100 Exponential 0.2) 0
    o = range 0 (blockSize - 1) (whiteNoise 'α' KR)
in ringz (t2a tr o) 880 0.1 * 0.4

---- ; drawings
UI.ui_sc3_scope 2 0 (2 ^ 14) 0 "audio" 0
