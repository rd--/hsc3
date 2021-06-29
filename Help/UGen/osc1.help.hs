-- osc1 ; requires=tbl ; composite UGen
let b0 = control kr "tbl0" 0
    b1 = control kr "tbl1" 1
    env = linLin (osc1 ar (mce2 b0 b1) 8 RemoveSynth) (-1) 1 0 1
in lfSaw ar (mce2 110 164) 0 * env * 0.1

---- ; setup
withSC3 (mapM_ maybe_async [b_alloc 0 4096 1,b_gen_sine1 0 [Normalise,Wavetable,Clear] (map recip [13,8,55,34,5,21,3,1,2])])
withSC3 (mapM_ maybe_async [b_alloc 1 4096 1,b_gen_sine1 1 [Normalise,Wavetable,Clear] (map recip [55,34,1,3,2,13,5,8,21])])

---- ; drawings
withSC3 (Sound.SC3.Plot.plot_wavetable 0)
withSC3 (Sound.SC3.Plot.plot_wavetable 1)
