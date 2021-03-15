-- RShufflerL
X.rShufflerL (sinOsc AR 1200 0 * 0.1) 0.02 0.04

-- RShufflerL ; mouse control
let x = mouseX KR 0.0001 0.02 Linear 0.2
    y = mouseY KR 0.001 0.25 Linear 0.2
    o = sinOsc AR (mce2 440 441) 0 * 0.2
in X.rShufflerL o x y

-- RShufflerL ; arrayed
let n = 6
    x = mouseX KR 0.0001 0.02 Linear 0.2
    y = mouseY KR 0.001 0.25 Linear 0.2
    f = x * X.rRandN n 'α' 0 4
    d = y * X.rRandN n 'β' 0 4
    o = sinOsc AR (mce2 440 441) 0 * 0.1
in splay (X.rShufflerL o f d) 1 1 0 True

---- ; drawings
Sound.SC3.Plot.plot_ugen_nrt (48000,64) 0.1 (X.rShufflerL (sinOsc AR 1200 0) 0.02 0.04)

{---- Linear input shuffler

A hamming window of duration 'fragmentSize' is applied to the input signal.
The inter-offset time is a linear random value between 0 and 'maxDelay'.
c.f. grainIn, monoGrain

-}
