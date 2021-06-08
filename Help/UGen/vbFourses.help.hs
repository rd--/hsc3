-- vbFourses ; a simple triangle wave ; supplying only one freq pair
let freqs = mce [150, 150]
    src = X.vbFourses AR 0.5 freqs
in leakDC (mceChannel 0 src) 0.995 * 0.4

-- vbFourses ; feeding it an array of frequencies ; mix to stereo
let freqs = mceFillInt 8 (\z -> rand z 1 500)
    src = X.vbFourses AR 0.1 freqs
in mixN 2 (leakDC src 0.995 * 0.3)

-- vbFourses ; modulate inputs ; splay
let freqs = lfNoise0 'α' AR (mce [4, 3, 2, 1, 1, 2, 3, 4]) `in_range` (1,1200)
    src = X.vbFourses AR 0.7 freqs
in splay (leakDC src 0.995) 1 0.3 0 True

-- vbFourses ; smoothing is an i-Rate input
let w = lfPulse AR 1 0 0.5 * 100 + 100
    freqs = mce [w, 200]
    src = X.vbFourses AR 0.9 freqs -- mouseX KR 0.4 0.9 Linear 0.1
in leakDC (mceChannel 0 src) 0.995 * 0.4

---- ; drawings
import Sound.SC3.Plot {- hsc3-plot -}
plot_ugen_nrt (48000,64) 1 (X.vbFourses AR 0.5 (mce [800, 30, 710, 3, 10, 1, 7, 11]))
