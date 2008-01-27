strummable guitar

> let { scale = [ 52, 57, 62, 67, 71, 76 ]
>     ; string i = let { x = mouseX KR 0 1 Linear 0.2
>                      ; t = abs (hpz1 (x >* (0.25 + constant i * 0.1)))
>                      ; e = decay t 0.05
>                      ; n = Sound.SC3.UGen.Base.pinkNoise (uid 0) AR * e
>                      ; dt = 1 / (midiCPS (scale !! i))
>                      ; s = combL n dt dt 4 }
>                  in pan2 s (constant i * 0.2 - 0.5) 1
>     ; a = mixFill (length scale) string }
> in audition (out 0 (leakDC (lpf a 12000) 0.995))

{ var pitch = [ 52, 57, 62, 67, 71, 76 ]
; var mousex = MouseX.kr
; var string = { arg i
               ; var trigger = HPZ1.kr(mousex > (0.25 + (i * 0.1))).abs
               ; var pluck = PinkNoise.ar(Decay.kr(trigger, 0.05))
	           ; var period = pitch.at(i).midicps.reciprocal
               ; var str = CombL.ar(pluck, period, period, 4)
               ; Pan2.ar(str, i * 0.2 - 0.5) }
; var out = Mix.arFill(pitch.size, string)
; LPF.ar(out, 12000)
; Out.ar(0, LeakDC.ar(out)) }.play

