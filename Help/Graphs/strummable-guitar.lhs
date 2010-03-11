strummable guitar (jmcc)

> import Sound.SC3.ID

> main =
>   let { scale = [ 52, 57, 62, 67, 71, 76 ]
>       ; str i = let { x = mouseX kr 0 1 Linear 0.2
>                     ; t = abs (hpz1 (x >* (0.25 + constant i * 0.1)))
>                     ; e = decay t 0.05
>                     ; n = pinkNoise i ar * e
>                     ; dt = 1 / (midiCPS (scale !! i))
>                     ; s = combL n dt dt 4 }
>                 in pan2 s (constant i * 0.2 - 0.5) 1
>       ; strs = mixFill (length scale) str }
>   in audition (out 0 (leakDC (lpf strs 12000) 0.995))

{ var scale = [ 52, 57, 62, 67, 71, 76 ]
; var str = { arg i
            ; var x = MouseX.kr(0, 1, 'linear', 0.2)
            ; var t = HPZ1.kr(x > (0.25 + (i * 0.1))).abs
            ; var e = Decay.kr(t, 0.05)
            ; var n = PinkNoise.ar * e
            ; var dt = scale.at(i).midicps.reciprocal
            ; var s = CombL.ar(n, dt, dt, 4)
            ; Pan2.ar(s, i * 0.2 - 0.5, 1) }
; var strs = Mix.fill(scale.size, str)
; Out.ar(0, LeakDC.ar(LPF.ar(strs, 12000), 0.995)) }.play
