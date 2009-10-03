shepard tones (alberto de campo)

> import Sound.SC3

> main =
>   let { indxs n l r = let i = (r - l) / n 
>                       in [l, l + i .. r - i]
>       ; hanningWindow n = 
>             let { lp = pi * (-0.5)
>                 ; rp = lp + 2 * pi 
>                 ; hf i = sin i * 0.5 + 0.5 }
>             in map hf (indxs n lp rp)
>       ; square x = x * x
>       ; ampTable = map square (hanningWindow 1024) 
>       ; amp_f i = (0.5 ** i) * 20000
>       ; freqTable = map amp_f (indxs 1024 0 10)
>       ; ratescale = 1024 / 44100 / 10
>       ; rate = 0.1
>       ; ph = phasor ar 0 (rate * ratescale) 0 1024 0
>       ; phases = mce (map (\n -> n * 0.1 * 1024 + ph) [0..9])
>       ; freqs = bufRdC 1 ar 1 phases Loop
>       ; amps = bufRdC 1 ar 2 phases Loop
>       ; tone = mix (sinOsc ar freqs 0 * amps) * 0.1 }
>   in withSC3 (\fd -> do { async fd (b_alloc 1 1024 1)
>                         ; async fd (b_alloc 2 1024 1)
>                         ; send fd (b_setn1 1 0 freqTable)
>                         ; send fd (b_setn1 2 0 ampTable)
>                         ; audition (out 0 tone) })

{ var ampTable = Signal.hanningWindow(1024).squared
; var amp_f = { arg i; 0.5 ** i * 20000 }
; var freqTable = Signal.newClear(1024).waveFill(amp_f, 0, 10)
; var b1 = Buffer.loadCollection(s, freqTable)
; var b2 = Buffer.loadCollection(s, ampTable)
; var ratescale = 1024 / 44100 / 10
; var rate = 0.1
; var ph = Phasor.ar(0, rate * ratescale, 0, 1024, 0)
; var phases = (0..9) * 0.1 * 1024 + ph
; var freqs = BufRd.ar(1, b1.bufnum, phases)
; var amps = BufRd.ar(1, b2.bufnum, phases)
; var tone = Mix.ar(SinOsc.ar(freqs) * amps) * 0.1 
; Out.ar(0, tone)}.play
