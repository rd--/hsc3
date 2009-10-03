moto rev (jmcc)

> import Sound.SC3

> main =
>   let { f = sinOsc kr 0.2 0 * 10 + 21
>       ; s = lfPulse ar f (mce2 0 0.1) 0.1 }
>   in audition (out 0 (clip2 (rlpf s 100 0.1) 0.4))

{ var f = SinOsc.kr(0.2, 0) * 10 + 21
; var s = LFPulse.ar(f, [0, 0.1], 0.1)
; Out.ar(0, RLPF.ar(s, 100, 0.1).clip2(0.4)) }.play
