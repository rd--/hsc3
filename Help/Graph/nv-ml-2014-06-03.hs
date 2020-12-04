-- sc-users, 2014-06-03 (nv) ; https://twitter.com/headcube/status/474064500564324352 (nv)
let y = mouseY KR 1 100 Linear 0.2
    x = mouseX KR 50 400 Exponential 0.2
    i = decay (impulse AR 0.5 0) 0.1
    i' = sin (bpf (i * y) 50 1)
    repl :: Int -> UGen -> UGen
    repl n = mce . replicate n
in repl 2 (pluck i' (lfSaw AR 10000 0) 0.1 (1 / x) 4 0.5)
