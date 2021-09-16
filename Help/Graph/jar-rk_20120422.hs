-- rukano; Apr 22, 2012; Re: Epic Pads (jar)
let freq_f _ = midiCps (lchoose [60,64,65,67]) * (lfNoise2 kr 1 * 0.011 + 1)
    freq = mceFill 24 freq_f
    gen = lfSaw ar freq 0 * 0.1
    fmod = lfCub kr (1/12) 0 `in_range` (1,mouseX kr 2 16 Linear 0.2)  -- modulate filter with mouse
    rqmod = lfNoise2 kr (1/8) `in_range` (0.1,1)
    sig = rlpf gen (freq * fmod) rqmod
    amp = mouseY kr 0 0.25 Linear 0.2
in splay sig 1 1 0 True * amp

-- rukano; Apr 22, 2012; Re: Epic Pads (jar) ; id
let freq_f z = midiCps (lchooseId z [60,64,65,67]) * (lfNoise2Id z kr 1 * 0.011 + 1)
    freq = mce (map freq_f (take 24 ['α'..]))
    gen = lfSaw ar freq 0 * 0.1
    fmod = lfCub kr (1/12) 0 `in_range` (1,mouseX kr 2 16 Linear 0.2)  -- modulate filter with mouse
    rqmod = lfNoise2Id 'β' kr (1/8) `in_range` (0.1,1)
    sig = rlpf gen (freq * fmod) rqmod
    amp = mouseY kr 0 0.25 Linear 0.2
in splay sig 1 1 0 True * amp
