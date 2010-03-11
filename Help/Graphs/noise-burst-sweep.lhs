noise burst sweep (jmcc)

> import Sound.SC3.Monadic

> main =
>   do { n <- clone 2 (whiteNoise ar)
>      ; let { lfoRate = mouseX kr 10 60 Exponential 0.2
>            ; amp = max 0 (lfSaw kr lfoRate (-1))
>            ; cfreq = mouseY kr 400 8000 Exponential 0.2
>            ; freq = sinOsc kr 0.2 0 * cfreq + (1.05 * cfreq) }
>        in audition (out 0 (resonz (n * amp) freq 0.1)) }

{ var n = WhiteNoise.ar ! 2
; var lfoRate = MouseX.kr(10, 60, 'exponential', 0.2)
; var amp = max(0, LFSaw.kr(lfoRate, -1))
; var cfreq = MouseY.kr(400, 8000, 'exponential', 0.2)
; var freq = SinOsc.kr(0.2, 0) * cfreq + (1.05 * cfreq)
; Out.ar(0, Resonz.ar(n * amp, freq, 0.1)) }.play
