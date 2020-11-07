-- bufRd ; phasor as phase input ; constructors bufRd{N|L|C} preset interpolation type
let nc = 2
    b = control KR "buf" 0
    tr = impulse AR (recip (bufDur KR b)) 0
    ph = phasor AR tr (bufRateScale KR b) 0 (bufFrames KR b) 0
in bufRdL nc AR b ph NoLoop

-- bufRd ; audio rate sine oscillator as phase input
let nc = 2
    b = control KR "buf" 0
    phase = sinOsc AR 0.1 0 * bufFrames KR b * bufRateScale KR b
in bufRdL nc AR b phase Loop

-- bufRd ; mouse control
let nc = 2
    b = control KR "buf" 0
    x = mouseX KR (mce [5, 10]) 100 Linear 0.1
    n = lfNoise1 'α' AR x
in mix (bufRdL nc AR b (n * bufFrames KR b * bufRateScale KR b) Loop)

-- bufRd ; fixed frequency wavetable oscillator ; c.f. osc
let nc = 1
    b = control KR "gen" 0
    x = mouseX KR 220 440 Exponential 0.2
    phase = linLin (lfSaw AR x 0) (-1) 1 0 1 * bufFrames KR b
in bufRdC nc AR b phase Loop * 0.1

---- ; setup ; nc=1
{buf = 0 ; fn = "/home/rohan/data/audio/metal.wav"}
withSC3 (async (b_allocRead buf fn 0 0))

---- ; setup ; nc=2
{buf = 0 ; fn = "/home/rohan/data/audio/pf-c5.aif"}
withSC3 (async (b_allocRead buf fn 0 0))

---- ; setup ; nc=1 gen=sine1 ; allocate and generate (non-wavetable) buffer
withSC3 (mapM_ maybe_async [b_alloc 0 256 1,b_gen_sine1 0 [Normalise,Clear] [1,1/2,1/3,1/4,1/5]])
