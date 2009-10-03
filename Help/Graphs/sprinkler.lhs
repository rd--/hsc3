sprinkler (jmcc)

> import Sound.SC3
> import qualified Sound.SC3.UGen.Base as B

> main =
>   do { n <- whiteNoise ar
>      ; let { f = lfPulse kr 0.09 0 0.16 * 10 + 7
>            ; t = lfPulse kr f 0 0.25 * 0.1 }
>        in audition (out 0 (bpz2 (n * t))) }

{ var f = LFPulse.kr(0.09, 0, 0.16, 10, 7)
; var t = LFPulse.kr(f, 0, 0.25, 0.1)
; Out.ar(0, BPZ2.ar(WhiteNoise.ar * t)) }.play

the same graph, with a non-monadic noise constructor

> alternate =
>   let { n = B.whiteNoise (uid 0) ar
>       ; f = lfPulse kr 0.09 0 0.16 * 10 + 7
>       ; t = lfPulse kr f 0 0.25 * 0.1 }
>   in audition (out 0 (bpz2 (n * t)))
