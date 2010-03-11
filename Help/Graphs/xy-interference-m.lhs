xy-interference-m (rd)

> import Sound.SC3.Monadic

> main =
>   let { x = mouseX KR 20 22000 Linear (mce2 0.005 0.025)
>       ; y = mouseY KR 20 22000 Linear (mce2 0.005 0.075)
>       ; nd = do { n <- lfNoise0 KR (mce2 5 9)
>                 ; let { a = sinOsc AR (x + n) 0
>                       ; b = sinOsc AR y 0 }
>                   in return (a * b) } }
>   in audition . (out 0) . sum =<< sequence (replicate 3 nd)

{ var x = MouseX.kr(20, 22000, 'linear', [0.005, 0.025])
; var y = MouseY.kr(20, 22000, 'linear', [0.005, 0.075])
; var nd = { var n = LFNoise0.kr([5, 9])
           ; var a = SinOsc.ar(x + n, 0)
           ; var b = SinOsc.ar(y, 0)
           ; a * b }
; Out.ar(0, Mix.fill(3, nd)) }.play
