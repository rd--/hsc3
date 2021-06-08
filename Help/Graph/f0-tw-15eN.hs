-- f0 ; https://sccode.org/1-5eN
let s = gbmanN AR (mce2 2300 1150) 1.2 2.1
    f0 = pulse AR 4 (mce2 1 2 / 8) + (lfPulse AR (1/8) 0 0.5 / 5 + 1)
    f1 = lfSaw AR f0 0 + 2
in X.glitchRHPF s f1 1 * 0.05
