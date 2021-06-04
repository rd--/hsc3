-- rukano; Apr 22, 2012; Re: Epic Pads (jar)
let freq_f z = midiCPS (lchoose z [60,64,65,67]) * (lfNoise2 z KR 1 * 0.011 + 1)
    freq = mce (map freq_f (take 24 ['α'..]))
    gen = lfSaw AR freq 0 * 0.1
    fmod = lfCub KR (1/12) 0 `in_range` (1,mouseX KR 2 16 Linear 0.2)  -- modulate filter with mouse
    rqmod = lfNoise2 'β' KR (1/8) `in_range` (0.1,1)
    sig = rlpf gen (freq * fmod) rqmod
    amp = mouseY KR 0 0.25 Linear 0.2
in splay sig 1 1 0 True * amp
