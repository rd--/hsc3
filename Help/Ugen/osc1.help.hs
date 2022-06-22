-- osc1 ; composite UGen
let tbl0 = asLocalBuf (Gen.sine1Tbl 2048 (map recip [13, 8, 55, 34, 5, 21, 3, 1, 2]))
    tbl1 = asLocalBuf (Gen.sine1Tbl 2048 (map recip [55, 34, 1, 3, 2, 13, 5, 8, 21]))
    env = linLin (osc1 ar (mce2 tbl0 tbl1) 8 RemoveSynth) (-1) 1 0 1
in lfSaw ar (mce2 110 164) 0 * env * 0.1

-- osc1 ; requires=tbl ; composite UGen
let b0 = control kr "tbl0" 0
    b1 = control kr "tbl1" 1
    env = linLin (osc1 ar (mce2 b0 b1) 8 RemoveSynth) (-1) 1 0 1
in lfSaw ar (mce2 110 164) 0 * env * 0.1

---- ; setup
withSc3 (mapM_ maybe_async [b_alloc 0 4096 1, b_gen_sine1 0 [Normalise, Wavetable, Clear] (map recip [13, 8, 55, 34, 5, 21, 3, 1, 2])])
withSc3 (mapM_ maybe_async [b_alloc 1 4096 1, b_gen_sine1 1 [Normalise, Wavetable, Clear] (map recip [55, 34, 1, 3, 2, 13, 5, 8, 21])])

---- ; drawings (buffers)
withSc3 (Sound.Sc3.Plot.plot_wavetable 0)
withSc3 (Sound.Sc3.Plot.plot_wavetable 1)

---- ; drawings (lists, pre wavetable transform)
Sound.Sc3.Plot.plot_p1_ln [Gen.sine1_nrm 2048 (map recip [13, 8, 55, 34, 5, 21, 3, 1, 2])]
Sound.Sc3.Plot.plot_p1_ln [Gen.sine1_nrm 2048 (map recip [55, 34, 1, 3, 2, 13, 5, 8, 21])]
