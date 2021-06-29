-- lorenzTrig ; randomly modulate params
let minfreq = 11025
    maxfreq = 44100
    s = lfNoise0 'α' kr 1 * 2 + 10
    b = lfNoise0 'β' kr 1 * 1.5 + 2
    r = 28
    h = 0.02
    x0 = 0.090879182417163
    y0 = 2.97077458055
    z0 = 24.282041054363
in X.lorenzTrig ar minfreq maxfreq s r b h x0 y0 z0 * 0.2

-- lorenzTrig ; as a frequency control
let x0 = 0.090879182417163
    y0 = 2.97077458055
    z0 = 24.282041054363
    n = X.lorenzTrig ar 1 8 10 28 28 0.02 x0 y0 z0
in sinOsc ar (decay n 1.0 * 800 + 900) 0 * 0.2
