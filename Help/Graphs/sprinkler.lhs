sprinkler (jmcc)

> do { n <- whiteNoise AR
>    ;  let { f = lfPulse KR 0.09 0 0.16 * 10 + 7
>           ; t = lfPulse KR f 0 0.25 * 0.1 }
>       in audition (out 0 (bpz2 (n * t))) }

{ var f = LFPulse.kr(0.09, 0, 0.16, 10, 7)
; var t = LFPulse.kr(f, 0, 0.25, 0.1)
; Out.ar(0, BPZ2.ar(WhiteNoise.ar * t)) }.play

with non-monadic noise

> let { n = Sound.SC3.UGen.Base.whiteNoise (uid 0) AR
>     ; f = lfPulse KR 0.09 0 0.16 * 10 + 7
>     ; t = lfPulse KR f 0 0.25 * 0.1 }
> in audition (out 0 (bpz2 (n * t)))
