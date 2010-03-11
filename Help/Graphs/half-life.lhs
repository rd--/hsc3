half-life (jrhb)

> import Sound.SC3.Monadic

> main =
>   let { t_half = 3.92
>       ; n_atoms = 1e+5
>       ; n = max 0 (n_atoms - pulseCount (localIn 2 ar) 0) }
>   in do { activity <- dust ar (n * log 2 / t_half)
>         ; audition (mrg [ localOut activity
>                         , out 0 activity ]) }

{ var t_half = 3.92
; var n_atoms = 1e+5
; var n = max(0, n_atoms - PulseCount.ar(LocalIn.ar(2), 0))
; var activity = Dust.ar(n * 2.log / t_half)
; LocalOut.ar(activity)
; Out.ar(0, activity) }.play
