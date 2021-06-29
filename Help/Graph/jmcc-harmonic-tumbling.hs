-- harmonic tumbling (jmcc) #1
let f = 80
    p = 10::Int
    t = xLine kr (mce2 10 11) 0.1 60 DoNothing
    o z = let n = dust z kr t
              r = rand z 0.25 0.5
              e = decay2 (n * 0.02) 0.005 r
          in fSinOsc ar (f * (constant z + 1)) 0 * e
in sum (map o [0 .. p])
