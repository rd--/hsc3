harmonic tumbling (jmcc)

> let { f = 80
>     ; p = 10
>     ; t = xLine KR (mce2 10 10) 0.1 60 DoNothing
>     ; o h = do { n <- dust KR t
>                ; r <- rand 0 0.5
>                ; let e = decay2 (n * 0.02) 0.005 r
>                  in return (fSinOsc AR (f * (h + 1)) 0 * e) } }
> in audition . out 0 . sum =<< mapM o [0..p]

{ var f = 80
; var p = 10
; var t = XLine.kr([10, 10], 0.1, 60, doneAction: 0)
; var o = { arg h
          ; var n = Dust.kr(t)
          ; var r = 0.5.rand
          ; var e = Decay2.kr(n * 0.02, 0.005, r)
          ; FSinOsc.ar(f * (h + 1), 0) * e }
; Out.ar(0, (0..p).collect(o).sum) }.play
