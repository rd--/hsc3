-- bufRd ; requires=buf ; phasor as phase input ; constructors bufRd{N|L|C} preset interpolation type
let (buf, nc) = (control kr "buf" 100, 2)
    tr = impulse ar (recip (bufDur kr buf)) 0
    ph = phasor ar tr (bufRateScale kr buf) 0 (bufFrames kr buf) 0
in bufRdL nc ar buf ph NoLoop

-- bufRd ; requires=buf ; audio rate sine oscillator as phase input
let (buf, nc) = (control kr "buf" 100, 2)
    phase = sinOsc ar 0.1 0 * bufFrames kr buf * bufRateScale kr buf
in bufRdL nc ar buf phase Loop

-- bufRd ; requires=buf ; mouse control
let (buf, nc) = (control kr "buf" 100, 2)
    x = mouseX kr (mce [5, 10]) 100 Linear 0.1
    n = lfNoise1Id 'α' ar x
in mix (bufRdL nc ar buf (n * bufFrames kr buf * bufRateScale kr buf) Loop)

-- bufRd ; fixed frequency wavetable oscillator ; c.f. osc
let (buf, nc) = (asLocalBuf (Gen.sine1_nrm 256 [1, 1/2, 1/3, 1/4, 1/5]), 1)
    x = mouseX kr 220 440 Exponential 0.2
    phase = linLin (lfSaw ar x 0) (-1) 1 0 1 * bufFrames kr buf
in bufRdC nc ar buf phase Loop * 0.1

-- bufRd ; requires=buf ; fixed frequency wavetable oscillator ; c.f. osc
let (buf, nc) = (control kr "tbl" 0, 1)
    x = mouseX kr 220 440 Exponential 0.2
    phase = linLin (lfSaw ar x 0) (-1) 1 0 1 * bufFrames kr buf
in bufRdC nc ar buf phase Loop * 0.1

---- ; setup ; nc=1
withSc3 (async (b_allocRead 0 (sfResolve "metal.wav") 0 0))

---- ; setup ; nc=2
withSc3 (async (b_allocRead 0 (sfResolve "pf-c5.aif") 0 0))
withSc3 (async (b_allocRead 0 (sfResolve "instr/bosendorfer/008/C5.aif") 0 0))
withSc3 (async (b_allocRead 0 (sfResolve "instr/bosendorfer/032/C4.aif") 0 0))

---- ; setup ; nc=1 gen=sine1 ; allocate and generate (non-wavetable) buffer
withSc3 (mapM_ maybe_async [b_alloc 0 256 1,b_gen_sine1 0 [Normalise, Clear] [1, 1/2, 1/3, 1/4, 1/5]])
