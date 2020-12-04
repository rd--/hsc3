-- coolant (jmcc) #2 ; texture=overlap,4,4,2,inf
let p = 20
    s = onePole (mce (map (\z -> brownNoise z AR * 0.0015) (id_seq p 'α'))) 0.95
    n = replicate p 1
    sp z = klanx_spec_f id id (map (\z' -> rand (z,z') 40 2400) (id_seq p 'β')) n n
in klank s 1 0 1 (mce (map mce (transpose (map sp ['γ','δ']))))
