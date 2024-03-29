-- osc ; fixed frequency wavetable oscillator ; c.f. bGen
let tbl = asLocalBuf (Gen.sine1Tbl 256 [1, 1/2, 1/3, 1/4, 1/5])
in osc ar tbl 220 0 * 0.1

-- osc ; mouse frequency control
let tbl = asLocalBuf (Gen.sine1Tbl 256 [1, 1/2, 1/3, 1/4, 1/5])
in osc ar tbl (mouseX kr 110 220 Exponential 0.2) 0 * 0.1

-- osc ; modulate frequency
let tbl = asLocalBuf (Gen.sine1Tbl 256 [1, 1/2, 1/3, 1/4, 1/5])
in osc ar tbl (xLine kr 2000 200 1 DoNothing) 0 * 0.1

-- osc ; as frequency modulator
let tbl = asLocalBuf (Gen.sine1Tbl 256 [1, 1/2, 1/3, 1/4, 1/5])
in osc ar tbl (osc ar tbl (xLine kr 1 1000 9 RemoveSynth) 0 * 200 + 800) 0 * 0.1

-- osc ; as phase modulator
let tbl = asLocalBuf (Gen.sine1Tbl 256 [1, 1/2, 1/3, 1/4, 1/5])
in osc ar tbl 800 (osc ar tbl (xLine kr 20 8000 10 RemoveSynth) 0 * 2 * pi) * 0.1

-- osc ; requires=tbl ; fixed frequency wavetable oscillator
let b = control kr "tbl" 0
in osc ar b 220 0 * 0.1

-- osc ; requires=tbl ; mouse frequency control
let b = control kr "tbl" 0
in osc ar b (mouseX kr 110 220 Exponential 0.2) 0 * 0.1

-- osc ; requires=tbl ;  modulate frequency
let b = control kr "tbl" 0
    f = xLine kr 2000 200 1 DoNothing
in osc ar b f 0 * 0.1

-- osc ; requires=tbl ; as frequency modulator
let b = control kr "tbl" 0
    f = osc ar b (xLine kr 1 1000 9 RemoveSynth) 0 * 200 + 800
in osc ar b f 0 * 0.1

-- osc ; requires=tbl ; as phase modulator
let b = control kr "tbl" 0
    p = osc ar b (xLine kr 20 8000 10 RemoveSynth) 0 * 2 * pi
in osc ar b 800 p * 0.1

---- ; setup ; allocate and generate wavetable buffer ; sin
withSc3 (mapM_ maybe_async [b_alloc 0 8192 1,b_gen_sine1 0 [Normalise,Wavetable,Clear] [1]])

---- ; setup ; allocate and generate wavetable buffer ; sin harmonics
withSc3 (mapM_ maybe_async [b_alloc 0 8192 1,b_gen_sine1 0 [Normalise,Wavetable,Clear] [1, 1/2, 1/3, 1/4, 1/5]])

---- ; setup ; change the wavetable while its playing
withSc3 (maybe_async (b_gen_sine1 0 [Normalise,Wavetable,Clear] [1, 0.6, 1/4]))

---- ; setup ; send calculated wavetable
import Sound.Sc3.Common.Math.Window {- hsc3 -}
let tbl = window_table TableOpen 4096 triangular
withSc3 (maybe_async (b_setn1 0 0 (Sound.Sc3.Common.Buffer.to_wavetable tbl)))

---- ; drawings
withSc3 (Sound.Sc3.Plot.plot_wavetable 0)
