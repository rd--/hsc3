-- cOsc ; fixed beat frequency ; requires=tbl
cOsc AR (control KR "tbl" 0) 200 0.7 * 0.1

-- cOsc ; modulate beat frequency with mouseX ; requires=tbl
cOsc AR (control KR "tbl" 0) 200 (mouseX KR 0 4 Linear 0.2) * 0.1

-- cOsc ; compare with plain osc ; requires=tbl
let f g = g AR (control KR "tbl" 0) 200 (mouseX KR 0 4 Linear 0.2)
in xFade2 (f cOsc) (f osc) (lfTri KR 0.1 0) 0.1

---- ; setup ; allocate and fill wavetable
withSC3 (mapM_ maybe_async [b_alloc 0 512 1,b_gen_sine1 0 [Normalise,Wavetable,Clear] [1,1/2,1/3,1/4,1/5,1/6,1/7,1/8,1/9,1/10]])

---- ; drawings
withSC3 (mapM_ maybe_async [b_alloc 1 512 1,b_gen_sine1 1 [Normalise,Wavetable,Clear] [1]])
Sound.SC3.Plot.plot_ugen1 0.1 (cOsc AR 1 100 5)
