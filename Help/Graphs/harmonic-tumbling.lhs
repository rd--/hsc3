harmonic tumbling (jmcc)

> import Sound.SC3.Monadic

> main =
>   let { f = 80
>       ; p = 10
>       ; t = xLine kr (mce2 10 11) 0.1 60 DoNothing
>       ; o h = do { n <- dust kr t
>                  ; r <- rand 0 0.5
>                  ; let e = decay2 (n * 0.02) 0.005 r
>                    in return (fSinOsc ar (f * (h + 1)) 0 * e) } }
>   in audition . out 0 . sum =<< mapM o [0..p]

{ var f = 80
; var p = 10
; var t = XLine.kr([10, 11], 0.1, 60, doneAction: 0)
; var o = { arg h
          ; var n = Dust.kr(t)
          ; var r = Rand.new(0, 0.5)
          ; var e = Decay2.kr(n * 0.02, 0.005, r)
          ; FSinOsc.ar(f * (h + 1), 0) * e }
; Out.ar(0, (0..p).collect(o).sum) }.play
