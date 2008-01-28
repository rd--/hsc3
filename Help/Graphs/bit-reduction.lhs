bit reduction (adc)

sample rate decrease

> do { f <- lfNoise2 KR 8
>    ; nh <- lfNoise2 KR 3
>    ; let { src = blip AR (f * 200 + 300) (nh * 10 + 20)
>          ; sr = mouseX KR 1000 (sampleRate * 0.1) Exponential 0.2 }
>      in audition (out 0 (latch src (impulse AR sr 0))) }

{ var f = LFNoise2.kr(8)
; var nh = LFNoise2.kr(3)
; var src = Blip.ar(f * 200 + 300, nh * 10 + 20)
; var sr = MouseX.kr(1000, s.sampleRate * 0.1, \exponential, 0.2)
; Out.ar(0, Latch.ar(src, Impulse.ar(sr, 0))) }.play

bit rate decrease

> do { f <- lfNoise2 KR 8
>    ; nh <- lfNoise2 KR 3
>    ; let { src = blip AR (f * 200 + 300) (nh * 10 + 20)
>          ; sr = mouseX KR 1000 (sampleRate * 0.1) Exponential 0.2
>          ; bit_sz = mouseY KR 1 24 Exponential 0.2
>          ; down_sample = latch src (impulse AR sr 0)
>          ; bit_redux = roundE down_sample (0.5 ** bit_sz) }
>      in audition (out 0 (mce2 down_sample bit_redux)) }

{ var f = LFNoise2.kr(8)
; var nh = LFNoise2.kr(3)
; var src = Blip.ar(f * 200 + 300, nh * 10 + 20)
; var sr = MouseX.kr(1000, s.sampleRate * 0.1, \exponential, 0.2)
; var bit_sz = MouseY.kr(1, 24, \exponential, 0.2) 
; var down_sample = Latch.ar(src, Impulse.ar(sr, 0))
; var bit_redux = down_sample.round(0.5 ** bit_sz)
; Out.ar(0, [down_sample, bit_redux]) }.play
