-- cOsc ; fixed beat frequency
let tbl = asLocalBuf (Gen.sine1Tbl 256 [1, 1/2, 1/3, 1/4, 1/5, 1/6, 1/7, 1/8, 1/9, 1/10])
in cOsc ar tbl 200 0.7 * 0.1

-- cOsc ; modulate beat frequency with mouseX
let tbl = asLocalBuf (Gen.sine1Tbl 256 [1, 1/2, 1/3, 1/4, 1/5, 1/6, 1/7, 1/8, 1/9, 1/10])
in cOsc ar tbl 200 (mouseX kr 0 4 Linear 0.2) * 0.1

-- cOsc ; compare with plain osc
let tbl = asLocalBuf (Gen.sine1Tbl 256 [1, 1/2, 1/3, 1/4, 1/5, 1/6, 1/7, 1/8, 1/9, 1/10])
    f g = g ar tbl 200 (mouseX kr 0 4 Linear 0.2)
in xFade2 (f cOsc) (f osc) (lfTri kr 0.1 0) 0.1

-- cOsc ; fixed beat frequency ; requires=tbl
cOsc ar (control kr "tbl" 0) 200 0.7 * 0.1

-- cOsc ; modulate beat frequency with mouseX ; requires=tbl
cOsc ar (control kr "tbl" 0) 200 (mouseX kr 0 4 Linear 0.2) * 0.1

-- cOsc ; compare with plain osc ; requires=tbl
let f g = g ar (control kr "tbl" 0) 200 (mouseX kr 0 4 Linear 0.2)
in xFade2 (f cOsc) (f osc) (lfTri kr 0.1 0) 0.1

---- ; setup ; allocate and fill wavetable
withSc3 (mapM_ maybe_async [b_alloc 0 512 1,b_gen_sine1 0 [Normalise,Wavetable,Clear] [1,1/2,1/3,1/4,1/5,1/6,1/7,1/8,1/9,1/10]])

---- ; drawings
withSc3 (mapM_ maybe_async [b_alloc 1 512 1,b_gen_sine1 1 [Normalise,Wavetable,Clear] [1]])
Sound.Sc3.Plot.plot_ugen1 0.1 (cOsc ar 1 100 5)
