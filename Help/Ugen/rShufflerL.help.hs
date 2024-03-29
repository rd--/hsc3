-- RShufflerL
X.rShufflerL (sinOsc ar 1200 0 * 0.1) 0.02 0.04

-- RShufflerL ; mouse control
let x = mouseX kr 0.0001 0.02 Linear 0.2
    y = mouseY kr 0.001 0.25 Linear 0.2
    o = sinOsc ar (mce2 440 441) 0 * 0.2
in X.rShufflerL o x y

-- RShufflerL ; arrayed
let n = 6
    x = mouseX kr 0.0001 0.02 Linear 0.2
    y = mouseY kr 0.001 0.25 Linear 0.2
    f = x * X.randNId n 'α' 0 4
    d = y * X.randNId n 'β' 0 4
    o = sinOsc ar (mce2 440 441) 0 * 0.1
in splay (X.rShufflerL o f d) 1 1 0 True

---- ; drawings
Sound.Sc3.Plot.plot_ugen_nrt (48000,64) 0.1 (X.rShufflerL (sinOsc ar 1200 0) 0.02 0.04)

{---- Linear input shuffler

A hamming window of durationId 'fragmentSize' is applied to the input signal.
The inter-offset time is a linear random value between 0 andId 'maxDelay'.
c.f. grainIn, monoGrain

-}
