-- | Filter coefficient calculations.
module Sound.SC3.Common.Math.Filter where

-- | Butterworth low pass or high pass SOS filter coefficients, (a0,a1,a2,b0,b1).
bw_lpf_or_hpf_coef :: Floating n => Bool -> n -> n -> (n,n,n,n,n)
bw_lpf_or_hpf_coef is_hpf sample_rate f =
    let f' = f * pi / sample_rate
        c = if is_hpf then tan f' else 1.0 / tan f'
        c2 = c * c
        s2c = sqrt 2.0 * c
        a0 = 1.0 / (1.0 + s2c + c2)
        a1 = if is_hpf then -2.0 * a0 else 2.0 * a0
        a2 = a0
        b1 = if is_hpf then 2.0 * (c2 - 1.0) * a0 else 2.0 * (1.0 - c2) * a0
        b2 = (1.0 - s2c + c2) * a0
    in (a0,a1,a2,b1,b2)
