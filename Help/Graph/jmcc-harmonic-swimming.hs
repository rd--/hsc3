-- harmonic swimming (jmcc) #1
let a = 0.02
    f = 50
    p = 20::Int
    l = line KR 0 (- a) 60 DoNothing
    o h = let r = X.randN 2 h 2 8
              n = lfNoise1 h KR r
              e = max 0 (n * a + l)
          in fSinOsc AR (f * (fromIntegral h + 1)) 0 * e
in sum (map o [0..p])
