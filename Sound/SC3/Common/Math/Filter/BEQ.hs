-- | BEQ filter coefficient calculations, results are (a0,a1,a2,b0,b1).
module Sound.SC3.Common.Math.Filter.BEQ where

-- | Calculate coefficients for bi-quad low pass filter.
bLowPassCoef :: Floating a => a -> a -> a -> (a,a,a,a,a)
bLowPassCoef sr freq rq =
    let w0 = pi * 2 * freq * (1 / sr)
        cos_w0 = cos w0
        i = 1 - cos_w0
        alpha = sin w0 * 0.5 * rq
        b0rz = recip (1 + alpha)
        a0 = i * 0.5 * b0rz
        a1 = i * b0rz
        b1 = cos_w0 * 2 * b0rz
        b2 = (1 - alpha) * negate b0rz
    in (a0,a1,a0,b1,b2)

-- | Calculate coefficients for bi-quad high pass filter.
bHiPassCoef :: Floating t => t -> t -> t -> (t, t, t, t, t)
bHiPassCoef sr freq rq =
  let w0 = pi * 2 * freq * (1 / sr)
      cos_w0 = cos w0
      i = 1 + cos_w0
      alpha = sin w0 * 0.5 * rq
      b0rz = recip (1 + alpha)
      a0 = i * 0.5 * b0rz
      a1 = negate i * b0rz
      b1 = cos_w0 * 2 * b0rz
      b2 = (1 - alpha) * negate b0rz
  in (a0, a1, a0, b1, b2)

bAllPassCoef :: Floating t => t -> t -> t -> (t, t, t, t, t)
bAllPassCoef sr freq rq =
  let w0 = pi * 2 * freq * (1 / sr)
      alpha = sin w0 * 0.5 * rq
      b0rz = recip (1 + alpha)
      a0 = (1 - alpha) * b0rz
      b1 = 2.0 * cos w0 * b0rz
  in (a0,negate b1, 1.0, b1,negate a0)

bBandPassCoef :: Floating t => t -> t -> t -> (t, t, t, t, t)
bBandPassCoef sr freq bw =
  let w0 = pi * 2 * freq * (1 / sr)
      sin_w0 = sin w0
      alpha = sin_w0 * sinh (0.34657359027997 * bw * w0 / sin_w0)
      b0rz = recip (1 + alpha)
      a0 = alpha * b0rz
      b1 = cos w0 * 2 * b0rz
      b2 = (1 - alpha) * negate b0rz
  in (a0, 0.0, negate a0, b1, b2)

bBandStopCoef :: Floating t => t -> t -> t -> (t, t, t, t, t)
bBandStopCoef sr freq bw =
  let w0 = pi * 2 * freq * (1 / sr)
      sin_w0 = sin w0
      alpha = sin_w0 * sinh (0.34657359027997 * bw * w0 / sin_w0)
      b0rz = recip (1 + alpha)
      b1 = 2.0 * cos w0 * b0rz
      b2 = (1 - alpha) * negate b0rz
  in (b0rz, negate b1, b0rz, b1, b2)

bPeakEQCoef :: Floating t => t -> t -> t -> t -> (t, t, t, t, t)
bPeakEQCoef sr freq rq db =
  let a = 10 ** (db / 40)
      w0 = pi * 2 * freq * (1 / sr)
      alpha = sin w0 * 0.5 * rq
      b0rz = recip (1 + (alpha / a))
      a0 = (1 + (alpha * a)) * b0rz
      a2 = (1 - (alpha * a)) * b0rz
      b1 = 2.0 * cos w0 * b0rz
      b2 = (1 - (alpha / a)) * negate b0rz
  in (a0, negate b1, a2, b1, b2)

bLowShelfCoef :: Floating t => t -> t -> t -> t -> (t, t, t, t, t)
bLowShelfCoef sr freq rs db =
  let a = 10 ** (db / 40)
      w0 = pi * 2 * freq * (1 / sr)
      cos_w0 = cos w0
      sin_w0 = sin w0
      alpha = sin_w0 * 0.5 * sqrt ((a + recip a) * (rs - 1) + 2.0)
      i = (a + 1) * cos_w0
      j = (a - 1) * cos_w0
      k = 2 * sqrt a * alpha
      b0rz = recip ((a + 1) + j + k)
      a0 = a * ((a + 1) - j + k) * b0rz
      a1 = 2 * a * ((a - 1) - i) * b0rz
      a2 = a * ((a + 1) - j - k) * b0rz
      b1 = 2.0 * ((a - 1) + i) * b0rz
      b2 = ((a + 1) + j - k) * negate b0rz
  in (a0, a1, a2, b1, b2)

bHiShelfCoef :: Floating t => t -> t -> t -> t -> (t, t, t, t, t)
bHiShelfCoef sr freq rs db =
  let a = 10 ** (db / 40)
      w0 = pi * 2 * freq * (1 / sr)
      cos_w0 = cos w0
      sin_w0 = sin w0
      alpha = sin_w0 * 0.5 * sqrt((a + recip a) * (rs - 1) + 2.0)
      i = (a+1) * cos_w0
      j = (a-1) * cos_w0
      k = 2 * sqrt(a) * alpha
      b0rz = recip ((a + 1) - j + k)
      a0 = a * ((a + 1) + j + k) * b0rz
      a1 = -2.0 * a * ((a - 1) + i) * b0rz
      a2 = a * ((a + 1) + j - k) * b0rz
      b1 = -2.0 * ((a - 1) - i) * b0rz
      b2 = ((a + 1) - j - k) * negate b0rz
  in (a0, a1, a2, b1, b2)
