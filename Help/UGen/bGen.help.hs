-- bGenSin1Tbl ; osc
let tbl = bGenSine1Tbl ("tbl", 0, 8192) [1, 1/2, 1/3, 1/4, 1/5]
in osc ar tbl 220 0 * 0.1

-- bGenSin1Tbl ; bufRd
let tbl = bGenSine1Tbl ("tbl", 0, 8192) [1, 1/2, 1/3, 1/4, 1/5]
    x = mouseX kr 220 440 Exponential 0.2
    phase = linLin (lfSaw ar x 0) (-1) 1 0 1 * bufFrames kr tbl
in bufRdC 1 ar tbl phase Loop * 0.1

-- bGenCheby ; shaper ; mouse control
let z = sinOsc ar 400 (pi / 2) * mouseY kr 0.01 1 Exponential 0.2
    t = bGenChebyTbl ("tbl", 0, 4096) [1, 0, 1, 1, 0, 1]
in shaper t z * 0.1

---- ; sine1 table setup ; allocate and generate wavetable buffer ; sin harmonics
withSC3 (mapM_ maybe_async [b_alloc 0 8192 1, b_gen_sine1 0 [Normalise,Wavetable,Clear] [1, 1/2, 1/3, 1/4, 1/5]])

---- ; print scsynth, the interpreter value that holds the reference that stores the end brackets
scsynthPrint scsynth
