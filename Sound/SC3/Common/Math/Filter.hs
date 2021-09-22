-- | Filter coefficient calculations.
module Sound.SC3.Common.Math.Filter where

-- | SOS filter coefficients, (a0,a1,a2,b1,b2)
type SosCoef t = (t,t,t,t,t)

-- | Butterworth low pass or high pass SOS filter coefficients, (a0,a1,a2,b1,b2).
bw_lpf_or_hpf_coef :: Floating n => Bool -> n -> n -> SosCoef n
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

-- | Two place infinite impulse response filter coefficients, (a0,b1,b2)
type Iir2Coef t = (t,t,t)

-- | rlpf coefficients, (a0,b1,b2).
rlpf_coef :: Floating n => (n -> n -> n) -> (n,n,n) -> Iir2Coef n
rlpf_coef max_f (radians_per_sample,f,rq) =
    let qr = max_f 0.001 rq
        pf = f * radians_per_sample
        d = tan (pf * qr * 0.5)
        c = (1.0 - d) / (1.0 + d)
        b1 = (1.0 + c) * cos pf
        b2 = negate c
        a0 = (1.0 + c - b1) * 0.25
    in (a0,b1,b2)

-- | resonz coefficients, (a0,b1,b2).
resonz_coef :: Floating n => (n,n,n) -> Iir2Coef n
resonz_coef (radians_per_sample,f,rq) =
    let ff = f * radians_per_sample
        b = ff * rq
        r = 1.0 - b * 0.5
        two_r = 2.0 * r
        r2 = r * r
        ct = (two_r * cos ff) / (1.0 + r2)
        b1 = two_r * ct
        b2 = negate r2
        a0 = (1.0 - r2) * 0.5
    in (a0,b1,b2)
