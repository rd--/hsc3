bit reduction (adc)

> import Sound.SC3.Monadic

> sample_rate_decrease =
>   do { f <- lfNoise2 kr 8
>      ; nh <- lfNoise2 kr 3
>      ; let { src = blip ar (f * 200 + 300) (nh * 10 + 20)
>            ; sr = mouseX kr 1000 (sampleRate * 0.1) Exponential 0.2 }
>        in audition (out 0 (latch src (impulse ar sr 0))) }

{ var f = LFNoise2.kr(8)
; var nh = LFNoise2.kr(3)
; var src = Blip.ar(f * 200 + 300, nh * 10 + 20)
; var sr = MouseX.kr(1000, s.sampleRate * 0.1, 'exponential', 0.2)
; Out.ar(0, Latch.ar(src, Impulse.ar(sr, 0))) }.play

> bit_rate_decrease =
>   do { f <- lfNoise2 kr 8
>      ; nh <- lfNoise2 kr 3
>      ; let { src = blip ar (f * 200 + 300) (nh * 10 + 20)
>            ; sr = mouseX kr 1000 (sampleRate * 0.1) Exponential 0.2
>            ; bit_sz = mouseY kr 1 24 Exponential 0.2
>            ; down_sample = latch src (impulse ar sr 0)
>            ; bit_redux = roundE down_sample (0.5 ** bit_sz) }
>        in audition (out 0 (mce2 down_sample bit_redux)) }

{ var f = LFNoise2.kr(8)
; var nh = LFNoise2.kr(3)
; var src = Blip.ar(f * 200 + 300, nh * 10 + 20)
; var sr = MouseX.kr(1000, s.sampleRate * 0.1, 'exponential', 0.2)
; var bit_sz = MouseY.kr(1, 24, 'exponential', 0.2) 
; var down_sample = Latch.ar(src, Impulse.ar(sr, 0))
; var bit_redux = down_sample.round(0.5 ** bit_sz)
; Out.ar(0, [down_sample, bit_redux]) }.play

> main = bit_rate_decrease
