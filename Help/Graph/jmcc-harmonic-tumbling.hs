-- harmonic tumbling (jmcc) #1
let f = 80
    p = 10
    t = xLine kr (mce2 10 11) 0.1 60 DoNothing
    o h = let n = dust kr t
              r = rand 0.25 0.5
              e = decay2 (n * 0.02) 0.005 r
          in fSinOsc ar (f * (h + 1)) 0 * e
in mixFill p o

-- harmonic tumbling (jmcc) #1 ; id
let f = 80
    p = 10::Int
    t = xLine kr (mce2 10 11) 0.1 60 DoNothing
    o z = let n = dustId z kr t
              r = randId z 0.25 0.5
              e = decay2 (n * 0.02) 0.005 r
          in fSinOsc ar (f * (constant z + 1)) 0 * e
in sum (map o [0 .. p])
