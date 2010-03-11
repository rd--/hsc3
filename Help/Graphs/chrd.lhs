chrd (rd)

> import Sound.SC3.Monadic

> main =
>   let chrd = do { r0 <- rand 0.05 0.5
>                 ; [r1, r2] <- sequence (replicate 2 (rand (-1) 1))
>                 ; r3 <- rand 0.15 0.35
>                 ; r4 <- rand 0.005 0.01
>                 ; let { m = mce [60, 65, 72, 77, 79, 84]
>                       ; ds = 3
>                       ; d = mce (map (* ds) [5, 4, 5, 7, 4, 5])
>                       ; f = midiCPS (xLine kr m (m + r0) d DoNothing)
>                       ; z = envTrapezoid 0 r3 d r4
>                       ; e = envGen kr 1 1 0 1 DoNothing z
>                       ; p = xLine kr r1 r2 d DoNothing
>                       ; o = fSinOsc ar f 0 }
>                   in return (mix (pan2 o p e)) }
>   in audition . out 0 . mix =<< clone 9 chrd

{ var chrd = { var r0 = Rand.new(0.05, 0.5)
             ; var r1 = Rand.new(-1, 1)
             ; var r2 = Rand.new(-1, 1)
             ; var r3 = Rand.new(0.15, 0.35)
             ; var r4 = Rand.new(0.005, 0.01)
             ; var m = [60, 65, 72, 77, 79, 84]
             ; var ds = 3
             ; var d = [5, 4, 5, 7, 4, 5] * ds
             ; var f = XLine.kr(m, m + r0, d).midicps
             ; var z_ = Env.linen(r3 * d, 0, (1 - r3) * d, r4)
             ; var z = Env.sine(d.maxItem, r4)
             ; var e = EnvGen.kr(z, 1, 1, 0, 1)
             ; var p = XLine.kr(r1, r2, d)
             ; var o = SinOsc.ar(f, 0)
             ; Mix.ar(Pan2.ar(o, p, e)) }
; Out.ar(0, Mix.fill(9, chrd)) }.play
