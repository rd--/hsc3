-- | Haskell implementations of SC3 UGens.
module Sound.SC3.UGen.HS where

-- | F = function, ST = state
type F_ST st i o = (i,st) -> (o,st)

-- | U = uniform
type F_U2 n = n -> n -> n
type F_U3 n = n -> n -> n -> n
type F_U4 n = n -> n -> n -> n -> n
type F_U5 n = n -> n -> n -> n -> n -> n
type F_U6 n = n -> n -> n -> n -> n -> n -> n
type F_U7 n = n -> n -> n -> n -> n -> n -> n -> n
type F_U8 n = n -> n -> n -> n -> n -> n -> n -> n -> n
type F_U9 n = n -> n -> n -> n -> n -> n -> n -> n -> n -> n

-- | T = tuple
type T2 n = (n,n)
type T3 n = (n,n,n)
type T4 n = (n,n,n,n)
type T5 n = (n,n,n,n,n)
type T6 n = (n,n,n,n,n,n)
type T7 n = (n,n,n,n,n,n,n)
type T8 n = (n,n,n,n,n,n,n,n)
type T9 n = (n,n,n,n,n,n,n,n,n)

-- | avg = average
avg2 :: Fractional n => F_U2 n
avg2 p q = (p + q) / 2

avg3 :: Fractional n => F_U3 n
avg3 p q r = (p + q + r) / 3

avg4 :: Fractional n => F_U4 n
avg4 p q r s = (p + q + r + s) / 4

avg5 :: Fractional n => F_U5 n
avg5 p q r s t = (p + q + r + s + t) / 5

avg9 :: Fractional n => F_U9 n
avg9 p q r s t u v w x = (p + q + r + s + t + u + v + w + x) / 9

-- | fir = finite impulse response
fir1 :: F_U2 n -> F_ST n n n
fir1 f (n,z0) = (f n z0,n)

fir2 :: F_U3 n -> F_ST (T2 n) n n
fir2 f (n,(z1,z0)) = (f n z0 z1,(z0,n))

fir3 :: F_U4 n -> F_ST (T3 n) n n
fir3 f (n,(z2,z1,z0)) = (f n z0 z1 z2,(z1,z0,n))

fir4 :: F_U5 n -> F_ST (T4 n) n n
fir4 f (n,(z3,z2,z1,z0)) = (f n z0 z1 z2 z3,(z2,z1,z0,n))

fir8 :: F_U9 n -> F_ST (T8 n) n n
fir8 f (n,(z7,z6,z5,z4,z3,z2,z1,z0)) = (f n z0 z1 z2 z3 z4 z5 z6 z7,(z6,z5,z4,z4,z2,z1,z0,n))

-- | iir = infinite impulse response
iir1 :: F_U2 n -> F_ST n n n
iir1 f (n,y0) = let r = f n y0 in (r,r)

iir2 :: F_U3 n -> F_ST (T2 n) n n
iir2 f (n,(y1,y0)) = let r = f n y0 y1 in (r,(y0,r))

biquad :: F_U5 n -> F_ST (T4 n) n n
biquad f (n,(x1,x0,y1,y0)) = let r = f n x0 x1 y0 y1 in (r,(x0,n,y0,r))

-- | sos = second order section
sos_f :: Num n => T5 n -> F_U5 n
sos_f (a0,a1,a2,b1,b2) x x1 x2 y1 y2 = a0*x + a1*x1 + a2*x2 - b1*y1 - b2*y2

sos :: Num n => T5 n -> F_ST (T4 n) n n
sos p = biquad (sos_f p)

-- | hp = high pass
hpz1 :: Fractional n => F_ST n n n
hpz1 = fir1 (\n z0 -> 0.5 * (n - z0))

hpz2 :: Fractional n => F_ST (T2 n) n n
hpz2 = fir2 (\n z0 z1 -> 0.25 * (n - (2 * z0) + z1))

-- | lp = low pass
lpz1 :: Fractional n => F_ST n n n
lpz1 = fir1 avg2

lpz2 :: Fractional n => F_ST (T2 n) n n
lpz2 = fir2 (\n z0 z1 -> 0.25 * (n + (2 * z0) + z1))

-- | bp = band pass
bpz2 :: Fractional n => F_ST (T2 n) n n
bpz2 = fir2 (\n _z0 z1 -> 0.5 * (n - z1))

-- | br = band reject
brz2 :: Fractional n => F_ST (T2 n) n n
brz2 = fir2 (\n _z0 z1 -> 0.5 * (n + z1))

-- | mavg = moving average
mavg5 :: Fractional n => F_ST (T4 n) n n
mavg5 = fir4 avg5

mavg9 :: Fractional n => F_ST (T8 n) n n
mavg9 = fir8 avg9

two_pi :: Floating n => n
two_pi = 2.0 * pi

-- | Sample rate (SR) to radians per sample (RPS).
--
-- > sr_to_rps 44100 == 0.00014247585730565955
sr_to_rps :: Floating n => n -> n
sr_to_rps sr = two_pi / sr

resonz_f :: Floating n => T3 n -> (n -> n -> n -> T2 n)
resonz_f (radians_per_sample,f,rq) x y1 y2 =
    let ff = f * radians_per_sample
        b = ff * rq
        r = 1.0 - b * 0.5
        two_r = 2.0 * r
        r2 = r * r
        ct = (two_r * cos ff) / (1.0 + r2)
        b1 = two_r * ct
        b2 = negate r2
        a0 = (1.0 - r2) * 0.5
        y0 = x + b1 * y1 + b2 * y2
    in (a0 * (y0 - y2),y0)

-- | ff = feed-forward, fb = feed-back
iir2_ff_fb :: (n -> n -> n -> T2 n) -> (n,T2 n) -> (n,T2 n)
iir2_ff_fb f (n,(y1,y0)) = let (r,y0') = f n y0 y1 in (r,(y0,y0'))

-- | ir = initialization rate
resonz_ir :: Floating n => T3 n -> F_ST (T2 n) n n
resonz_ir p = iir2_ff_fb (resonz_f p)

-- | rlp = resonant low pass
rlpf_f :: (Floating n, Ord n) => T3 n -> F_U3 n
rlpf_f (radians_per_sample,f,rq) x y1 y2 =
    let qr = max 0.001 rq
        pf = f * radians_per_sample
        d = tan (pf * qr * 0.5)
        c = (1.0 - d) / (1.0 + d)
        b1 = (1.0 + c) * cos pf
        b2 = negate c
        a0 = (1.0 + c - b1) * 0.25
    in a0 * x + b1 * y1 + b2 * y2

rlpf_ir :: (Floating n, Ord n) => T3 n -> F_ST (T2 n) n n
rlpf_ir p = iir2 (rlpf_f p)

bw_lpf_or_hpf_coef :: Floating n => Bool -> n -> n -> T5 n
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

bw_hpf_ir :: Floating n => T2 n -> F_ST (T4 n) n n
bw_hpf_ir (sample_rate,f) = sos (bw_lpf_or_hpf_coef True sample_rate f)

bw_lpf_ir :: Floating n => T2 n -> F_ST (T4 n) n n
bw_lpf_ir (sample_rate,f) = sos (bw_lpf_or_hpf_coef False sample_rate f)

-- | * LIST PROCESSING

l_apply :: F_ST st i o -> st -> [i] -> [o]
l_apply f st xs =
    case xs of
      [] -> []
      x:xs' -> let (r,st') = f (x,st) in r : l_apply f st' xs'

l_hpz1 :: Fractional n => [n] -> [n]
l_hpz1 = l_apply hpz1 0

l_hpz2 :: Fractional n => [n] -> [n]
l_hpz2 = l_apply hpz2 (0,0)

l_lpz1 :: Fractional n => [n] -> [n]
l_lpz1 = l_apply lpz1 0

l_lpz2 :: Fractional n => [n] -> [n]
l_lpz2 = l_apply lpz2 (0,0)

l_bpz2 :: Fractional n => [n] -> [n]
l_bpz2 = l_apply bpz2 (0,0)

l_brz2 :: Fractional n => [n] -> [n]
l_brz2 = l_apply brz2 (0,0)

l_bw_hpf :: Floating n => T2 n -> [n] -> [n]
l_bw_hpf p = l_apply (bw_hpf_ir p) (0,0,0,0)

l_bw_lpf :: Floating n => T2 n -> [n] -> [n]
l_bw_lpf p = l_apply (bw_lpf_ir p) (0,0,0,0)

l_resonz_ir :: Floating n => T3 n -> [n] -> [n]
l_resonz_ir p = l_apply (resonz_ir p) (0,0)

l_rlpf_ir :: (Floating n, Ord n) => T3 n -> [n] -> [n]
l_rlpf_ir p = l_apply (rlpf_ir p) (0,0)

l_mavg5 :: Fractional n => [n] -> [n]
l_mavg5 = l_apply mavg5 (0,0,0,0)

l_mavg9 :: Fractional n => [n] -> [n]
l_mavg9 = l_apply mavg9 (0,0,0,0,0,0,0,0)

{-
import Sound.SC3.Lang.Random.ID {- hsc3-lang -}
import Sound.SC3.Plot {- hsc3-plot -}
import Sound.SC3.Plot.FFT {- hsc3-plot -}

let n = nrrand 'a' 4096 (-1.0) 1.0

plotTable1 n
plotTable1 (l_lpz1 n)
plotTable1 (l_lpz2 n)
plotTable1 (l_hpz1 n)
plotTable1 (l_hpz2 n)

plotTable1 (rfft_pure n)
plotTable1 (rfft_pure (l_lpz1 n))
plotTable1 (rfft_pure (l_lpz2 n))
plotTable1 (rfft_pure (l_hpz1 n))
plotTable1 (rfft_pure (l_hpz2 n))
plotTable1 (rfft_pure (l_bpz2 n))
plotTable1 (rfft_pure (l_brz2 n))
plotTable1 (rfft_pure (l_bw_lpf (44100,9000) n))
plotTable1 (rfft_pure (l_bw_hpf (44100,9000) n))
plotTable1 (rfft_pure (l_resonz_ir (sr_to_rps 44100,440,0.1) n))
plotTable1 (rfft_pure (l_rlpf_ir (sr_to_rps 44100,1200,0.1) n))

plotTable1 (l_mavg9 (rfft_pure n))
plotTable1 (l_mavg9 (rfft_pure (l_lpz2 n)))
plotTable1 (l_mavg9 (rfft_pure (l_hpz2 n)))
plotTable1 (l_mavg9 (rfft_pure (l_bpz2 n)))
plotTable1 (l_mavg9 (l_mavg9 (l_mavg9 (l_mavg9 (rfft_pure (l_brz2 n))))))

-}
