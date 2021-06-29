-- vOsc ; oscillator at eight consecutive wavetable buffers, mouse selects ; requires=tbl
let n = 8
    b = control kr "tbl" 0
    x = mouseX kr b (b + n - 1) Linear 0.1
    y = mouseY kr 0.01 0.2 Exponential 0.2
in vOsc ar x (mce [120, 121]) 0 * y

---- ; setup ; allocate and fill tables 0 to 7
gen_harm i = let {square a = a * a ; n = square (fromIntegral i + 1) ; f j = square ((n - j) / n) } in map f [0 .. n - 1]
gen_setup i = [b_alloc i 1024 1,b_gen_sine1 i [Normalise,Wavetable,Clear] (gen_harm i)]
withSC3 (mapM_ maybe_async (concatMap gen_setup [0 .. 7]))

---- ; drawings
Sound.SC3.Plot.plot_p1_imp [gen_harm 7]

---- ; setup ; reallocate buffers while oscillator is running
resetTable i = do {h <- Sound.SC3.Lang.Random.IO.nrrand 12 0 1; Sound.OSC.sendMessage (b_gen_sine1 i b_flags h)}
withSC3 (mapM_ resetTable [0 .. 7])
