-- why supercollider (jmcc) #0
let r z = resonz (dust z AR 0.2 * 50) (rand z 200 3200) 0.003
    s = mix (mce_gen r 10 'α')
    c z = combL (delayN s 0.048 0.048) 0.1 (lfNoise1 z KR (rand z 0 0.1) * 0.04 + 0.05) 15
    y = mix (mce_gen c 7 'β')
    f z i = allpassN i 0.05 (mce2 (rand (z,'γ') 0 0.05) (rand (z,'δ') 0 0.05)) 1
    x = foldl (&) y (map f (id_seq 4 'ε'))
in s + 0.2 * x
