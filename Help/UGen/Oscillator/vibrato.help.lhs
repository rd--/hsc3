> Sound.SC3.UGen.Help.viewSC3Help "Vibrato"
> Sound.SC3.UGen.DB.ugenSummary "Vibrato"

> import Sound.SC3

vibrato at 1 Hz, note the use of DC.ar

{SinOsc.ar(Vibrato.ar(DC.ar(400.0),1,0.02))*0.1}.play

> let v = vibrato AR (dc AR 400) 1 0.02 0 0 0.04 0.1 0
> in audition (out 0 (sinOsc AR v 0 * 0.1))

compare: k-rate freq input can be a constant

{SinOsc.ar(Vibrato.kr(400.0,1,0.02))}.play

> let v = vibrato KR 400 1 0.02 0 0 0.04 0.1 0
> in audition (out 0 (sinOsc AR v 0 * 0.1))

control rate and rateVariation
{x=MouseX.kr(2.0,100.0)
;y=MouseY.kr(0.0,1.0)
;v=Vibrato.ar(DC.ar(400.0),x,0.1,1.0,1.0,y,0.1)
;SinOsc.ar(v)}.play
> let {x = mouseX KR 2 100 Linear 0.2
>     ;y = mouseY KR 0 1 Linear 0.2
>     ;v = vibrato AR (dc AR 400) x 0.1 1 1 y 0.1 0}
> in audition (out 0 (sinOsc AR v 0 * 0.1))

control depth and depthVariation

{n=LFNoise1.kr(1,3,7)
;x=MouseX.kr(0.0,1.0)
;y=MouseY.kr(0.0,1.0)
;v=Vibrato.ar(DC.ar(400.0),n,x,1.0,1.0,y,0.1)
;SinOsc.ar(v)}.play

> let {n = lfNoise1 'a' KR 1 * 3 + 7
>     ;x = mouseX KR 0 1 Linear 0.2
>     ;y = mouseY KR 0 1 Linear 0.2
>     ;v = vibrato AR (dc AR 400) n x 1 1 y 0.1 0}
> in audition (out 0 (sinOsc AR v 0 * 0.1))
