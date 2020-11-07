-- osc ; fixed frequency wavetable oscillator
let b = control KR "tbl" 0
in osc AR b 220 0 * 0.1

-- osc ; mouse frequency control
let b = control KR "tbl" 0
in osc AR b (mouseX KR 110 220 Exponential 0.2) 0 * 0.1

-- osc ; modulate frequency
let b = control KR "tbl" 0
    f = xLine KR 2000 200 1 DoNothing
in osc AR b f 0 * 0.1

-- osc ; as frequency modulator
let b = control KR "tbl" 0
    f = osc AR b (xLine KR 1 1000 9 RemoveSynth) 0 * 200 + 800
in osc AR b f 0 * 0.1

-- osc ; as phase modulator
let b = control KR "tbl" 0
    p = osc AR b (xLine KR 20 8000 10 RemoveSynth) 0 * 2 * pi
in osc AR b 800 p * 0.1

---- ; setup ; allocate and generate wavetable buffer ; sin
withSC3 (mapM_ maybe_async [b_alloc 0 8192 1,b_gen_sine1 0 [Normalise,Wavetable,Clear] [1]])

---- ; setup ; allocate and generate wavetable buffer ; sin harmonics
withSC3 (mapM_ maybe_async [b_alloc 0 8192 1,b_gen_sine1 0 [Normalise,Wavetable,Clear] [1,1/2,1/3,1/4,1/5]])

---- ; setup ; change the wavetable while its playing
withSC3 (maybe_async (b_gen_sine1 0 [Normalise,Wavetable,Clear] [1,0.6,1/4]))

---- ; setup ; send calculated wavetable
withSC3 (maybe_async (b_setn1 0 0 (Sound.SC3.Common.Buffer.to_wavetable (Sound.SC3.Common.Math.Window.triangular_table 4096))))

---- ; drawings
withSC3 (Sound.SC3.Plot.plot_wavetable 0)
