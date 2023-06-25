-- | Filter coefficient calculations.
module Sound.Sc3.Common.Math.Filter where

-- | Sos filter coefficients, (a0, a1, a2, b1, b2)
type SosCoef t = (t,t,t,t,t)

-- | Butterworth low pass or high pass Sos filter coefficients.
bw_lpf_or_hpf_coef :: Floating n => Bool -> n -> n -> SosCoef n
bw_lpf_or_hpf_coef is_hpf sample_rate f =
    let f' = f * pi / sample_rate
        c = if is_hpf then tan f' else 1 / tan f'
        c2 = c * c
        s2c = sqrt 2 * c
        a0 = 1 / (1 + s2c + c2)
        a1 = if is_hpf then -2 * a0 else 2 * a0
        a2 = a0
        b1 = if is_hpf then 2 * (c2 - 1.0) * a0 else 2 * (1 - c2) * a0
        b2 = (1 - s2c + c2) * a0
    in (a0,a1,a2,b1,b2)

-- | Two place infinite impulse response filter coefficients, (a0, b1, b2)
type Iir2Coef t = (t,t,t)

-- | rlpf coefficients, (a0,b1,b2).
rlpf_coef :: Floating n => (n -> n -> n) -> (n,n,n) -> Iir2Coef n
rlpf_coef max_f (radians_per_sample,f,rq) =
    let qr = max_f 0.001 rq
        pf = f * radians_per_sample
        d = tan (pf * qr * 0.5)
        c = (1 - d) / (1 + d)
        b1 = (1 + c) * cos pf
        b2 = negate c
        a0 = (1 + c - b1) * 0.25
    in (a0,b1,b2)

-- | resonz coefficients, (a0,b1,b2).
resonz_coef :: Floating n => (n,n,n) -> Iir2Coef n
resonz_coef (radians_per_sample,f,rq) =
    let ff = f * radians_per_sample
        b = ff * rq
        r = 1 - b * 0.5
        two_r = 2 * r
        r2 = r * r
        ct = (two_r * cos ff) / (1 + r2)
        b1 = two_r * ct
        b2 = negate r2
        a0 = (1 - r2) * 0.5
    in (a0,b1,b2)

-- * Pinking

type Pinking_Param t = (t, t, t, t, t, t, t)

{- | Sample rate variable pinking filter.

https://www.musicdsp.org/en/latest/Filters/76-pink-noise-filter.html
-}
pinking_filter_freq_48000 :: Fractional t => Pinking_Param t
pinking_filter_freq_48000 = (4752.456, 4030.961, 2784.711, 1538.461, 357.681, 70, 30)

pinking_filter_freq_96000 :: Fractional t => Pinking_Param t
pinking_filter_freq_96000 = (8227.219, 8227.219, 6388.570, 3302.754, 479.412, 151.070, 54.264)

pinking_filter_freq_192000 :: Fractional t => Pinking_Param t
pinking_filter_freq_192000 = (9211.912, 8621.096, 8555.228, 8292.754, 518.334, 163.712, 240.241)

{- | Pinking filter coefficients

>>> pinking_filter_coef 48000 pinking_filter_freq_48000
(0.5368186045507747,0.5899888969306109,0.6945314610687594,0.8175983529924599,0.9542588306160661,0.9908788735874952,0.9960807097281633)
-}
pinking_filter_coef :: Floating t => t -> Pinking_Param t -> Pinking_Param t
pinking_filter_coef sr (f0, f1, f2, f3, f4, f5, f6) =
  let f n = exp ((- 2) * pi * n / sr)
  in (f f0, f f1, f f2, f f3, f f4, f f5, f f6)

pinking_filter_next :: Floating t => Pinking_Param t -> Pinking_Param t -> t -> (t, Pinking_Param t)
pinking_filter_next (k0, k1, k2, k3, k4, k5, k6) (m0, m1, m2, m3, m4, m5, m6) white =
  let (b0, b1, b2, b3, b4, b5, b6) = ((k0 * white) + (k0 * m0)
                                     ,(k1 * white) + (k1 * m1)
                                     ,(k2 * white) + (k2 * m2)
                                     ,(k3 * white) + (k3 * m3)
                                     ,(k4 * white) + (k4 * m4)
                                     ,(k5 * white) + (k5 * m5)
                                     ,(k6 * white) + (k6 * m6))
      pink = b0 + b1 + b2 + b3 + b4 + b5 + white - b6
  in (pink, (b0, b1, b2, b3, b4, b5, b6))
