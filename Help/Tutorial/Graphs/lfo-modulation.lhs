lfo modulation of pulse waves and resonant filters

> let { o = fSinOsc KR 0.05 0 * 80 + 160
>     ; p = fSinOsc KR (mce2 0.6 0.7) 0 * 3600 + 4000
>     ; s = rlpf (lfPulse AR o 0 0.4 * 0.05) p 0.2 }
> in audition (out 0 (combL s 0.3 (mce2 0.2 0.25) 2))

{ var o = FSinOsc.kr(0.05, 0, 80, 160)
; var p = FSinOsc.kr([0.6, 0.7], 0, 3600, 4000)
; var s = RLPF.ar(LFPulse.ar(o, 0, 0.4, 0.05), p, 0.2)
; Out.ar(0, CombL.ar(s, 0.3, [0.2, 0.25], 2)) }.play
