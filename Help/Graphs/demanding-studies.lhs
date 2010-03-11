demanding studies (jmcc)

> import Sound.SC3.Monadic

> main =
>   do { s1 <- drand dinf (mce [72, 75, 79, 82])
>      ; s2 <- drand 1 (mce [82, 84, 86])
>      ; s3 <- dseq dinf (mce [72, 75, 79, s2])
>      ; let { x = mouseX kr 5 13 Linear 0.2
>            ; tr = impulse kr x 0
>            ; f = demand tr 0 (mce [midiCPS (s1 - 12), midiCPS s3])
>            ; o1 = sinOsc ar (f + mce2 0 0.7) 0
>            ; o2 = saw ar (f + mce2 0 0.7) * 0.3
>            ; o3 = cubed (distort (log (distort (o1 + o2)))) }
>        in audition (out 0 (o3 * 0.1)) }

{ var s1 = Drand.new([72, 75, 79, 82], inf)
; var s2 = Drand.new([82, 84, 86], 1)
; var s3 = Dseq.new([72, 75, 79, s2], inf)
; var x = MouseX.kr(5, 13, 'linear', 0.2)
; var tr = Impulse.kr(x, 0)
; var f = Demand.kr(tr, 0, [(s1 - 12).midicps, s3.midicps])
; var o1 = SinOsc.ar(f + [0, 0.7], 0)
; var o2 = Saw.ar(f + [0, 0.7]) * 0.3
; var o3 = (o1 + o2).distort.log.distort.cubed
; Out.ar(0, o3 * 0.1) }.play
