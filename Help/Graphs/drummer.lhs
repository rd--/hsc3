drummer (thor magnusson)

> do { n <- whiteNoise AR
>    ; let { tempo = 4
>          ; dup a = mce2 a a 
>          ; tr = impulse AR tempo 0
>          ; tr_2 = pulseDivider tr 4 2
>          ; tr_4 = pulseDivider tr 4 0
>          ; snare = n * decay2 tr_2 0.005 0.5
>          ; bass = sinOsc AR 60 0 * decay2 tr_4 0.005 0.5
>          ; hihat = hpf n 10000 * decay2 tr 0.005 0.5 }
>      in audition (out 0 (pan2 (snare + bass + hihat) 0 0.4)) }

{ var tempo = 4
; var n = WhiteNoise.ar()
; var tr = Impulse.ar(tempo, 0)
; var tr_2 = PulseDivider.ar(tr, 4, 2)
; var tr_4 = PulseDivider.ar(tr, 4, 0)
; var snare = n * Decay2.ar(tr_2, 0.005, 0.5)
; var bass = SinOsc.ar(60, 0) * Decay2.ar(tr_4, 0.005, 0.5)
; var hihat = HPF.ar(n, 10000) * Decay2.ar(tr, 0.005, 0.5)
; Out.ar(0, Pan2.ar(snare + bass + hihat, 0, 0.4)) }.play

(let* ((tempo 4)
       (n (WhiteNoise ar))
       (tr (Impulse ar tempo 0))
       (tr_2 (PulseDivider tr 4 2))
       (tr_4 (PulseDivider tr 4 0))
       (snare (Mul n (Decay2 tr_2 0.005 0.5)))
       (bass (Mul (SinOsc ar 60 0) (Decay2 tr_4 0.005 0.5)))
       (hihat (Mul (HPF n 10000) (Decay2 tr 0.005 0.5))))
  (audition (Out 0 (Pan2 (Add3 snare bass hihat) 0 0.4))))
