record scratcher (josh parmenter)

> import Sound.SC3

> main =
>   let { dup a = mce2 a a
>       ; fn = "/home/rohan/audio/metal.wav"
>       ; d = env [0, 1, 0] [0.1, 0.1] [EnvSin] 1 0
>       ; e = envGen kr 1 1 0 1 RemoveSynth d
>       ; x = mouseX kr (-10) 10 Linear 0.2
>       ; dx = x - delayN x 0.1 0.1
>       ; bdx = mouseButton kr 1 0 0.3 + dx
>       ; bdxr = bdx * bufRateScale kr 0
>       ; scr = playBuf 1 0 bdxr 0 0 Loop DoNothing }
>   in withSC3 (\fd -> do { async fd (b_allocRead 0 fn 0 0)
>                         ; play fd (out 0 (dup (scr * e))) })

{ var fn = "/home/rohan/audio/metal.wav"
; var b = 0
; var gate = 1
; var e = Env.new([0, 1, 0], [0.1, 0.1], \sin, 1, nil)
; var env = EnvGen.kr(e, gate, doneAction: 2)
; var x = MouseX.kr(-10, 10, 'linear', 0.2)
; var dx = x - DelayN.kr(x, 0.1, 0.1)
; var bdx = MouseButton.kr(1, 0, 0.3) + dx
; var bdxr = bdx * BufRateScale.kr(b)
; var scr = PlayBuf.ar(1, b, bdxr, 0, 0, 1)
; var s = Server.default
; s.sendMsg("/b_allocRead", b, fn, 0, 0)
; Out.ar(0, (scr * env).dup ) }.play
