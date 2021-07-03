-- sc-users, 2014-06-03 (nv) ; https://twitter.com/headcube/status/474064500564324352 (nv)
let y = mouseY kr 1 100 Linear 0.2
    x = mouseX kr 50 400 Exponential 0.2
    i = decay (impulse ar 0.5 0) 0.1
    i' = sin (bpf (i * y) 50 1)
    repl n = mce . replicate n
in repl 2 (pluck i' (lfSaw ar 10000 0) 0.1 (1 / x) 4 0.5)
