> Sound.SC3.UGen.Help.viewSC3Help "BPF"
> Sound.SC3.UGen.DB.ugenSummary "BPF"

> import Sound.SC3.ID

> let f = fSinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
> in audition (out 0 (bpf (saw AR 200 * 0.5) f 0.3 ))

> let { n = whiteNoise 'a' AR
>     ; x = mouseX' KR 220 440 Exponential 0.1
>     ; y = mouseY' KR 0.01 0.2 Linear 0.1 }
> in audition (out 0 (bpf n (mce [x, 550 - x]) y))
