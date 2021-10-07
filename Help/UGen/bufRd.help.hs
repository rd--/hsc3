-- bufRd ; requires=buf ; phasor as phase input ; constructors bufRd{N|L|C} preset interpolation type
let nc = 2
    b = control kr "buf" 0
    tr = impulse ar (recip (bufDur kr b)) 0
    ph = phasor ar tr (bufRateScale kr b) 0 (bufFrames kr b) 0
in bufRdL nc ar b ph NoLoop

-- bufRd ; requires=buf ; audio rate sine oscillator as phase input
let nc = 2
    b = control kr "buf" 0
    phase = sinOsc ar 0.1 0 * bufFrames kr b * bufRateScale kr b
in bufRdL nc ar b phase Loop

-- bufRd ; requires=buf ; mouse control
let nc = 2
    b = control kr "buf" 0
    x = mouseX kr (mce [5, 10]) 100 Linear 0.1
    n = lfNoise1Id 'α' ar x
in mix (bufRdL nc ar b (n * bufFrames kr b * bufRateScale kr b) Loop)

-- bufRd ; fixed frequency wavetable oscillator ; c.f. osc
let nc = 1
    b = asLocalBuf (Gen.sine1_nrm 256 [1, 1/2, 1/3, 1/4, 1/5])
    x = mouseX kr 220 440 Exponential 0.2
    phase = linLin (lfSaw ar x 0) (-1) 1 0 1 * bufFrames kr b
in bufRdC nc ar b phase Loop * 0.1

-- bufRd ; requires=buf ; fixed frequency wavetable oscillator ; c.f. osc
let nc = 1
    b = control kr "gen" 0
    x = mouseX kr 220 440 Exponential 0.2
    phase = linLin (lfSaw ar x 0) (-1) 1 0 1 * bufFrames kr b
in bufRdC nc ar b phase Loop * 0.1

---- ; setup ; nc=1
{buf = 0 ; fn = sfResolve "metal.wav"}
withSC3 (async (b_allocRead buf fn 0 0))

---- ; setup ; nc=2
{buf = 0 ; fn = sfResolve "pf-c5.aif"}
withSC3 (async (b_allocRead buf fn 0 0))

---- ; setup ; nc=1 gen=sine1 ; allocate and generate (non-wavetable) buffer
withSC3 (mapM_ maybe_async [b_alloc 0 256 1,b_gen_sine1 0 [Normalise, Clear] [1, 1/2, 1/3, 1/4, 1/5]])
