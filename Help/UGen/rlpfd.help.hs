-- rlpfd
let s = mix (lfSaw ar (mce2 120 180) 0 * 0.33)
    f = linExp (lfCub kr 0.1 (0.5 * pi)) (-1) 1 280 1500
in X.rlpfd s f 0.6 0.5
