> Sound.SC3.UGen.Help.viewSC3Help "Balance2"
> Sound.SC3.UGen.DB.ugenSummary "Balance2"

> import Sound.SC3.ID

{Balance2.ar(LFSaw.ar(44),Pulse.ar(33),FSinOsc.kr(0.5),0.1)}.play
> let {l = lfSaw AR 44 0
>     ;r = pulse AR 33 0.5
>     ;p = fSinOsc KR 0.5 0
>     ;o = balance2 l r p 0.1}
> in audition (out 0 o)

{var s=SinOsc.ar([440,550]);Balance2.ar(s[0],s[1],LFNoise0.kr(4),0.3)}.play
> let {MCE [s0,s1] = sinOsc AR (mce2 440 550) 0
>     ;n = lfNoise0 'a' KR 4
>     ;o = balance2 s0 s1 n 0.3}
> in audition (out 0 o)

{var s=SinOsc.ar(440);Out.ar(0,0.2*Balance2.ar(s,s,SinOsc.kr(0.2)))}.play
> let {s = sinOsc AR 440 0
>     ;p = sinOsc KR 0.2 0
>     ;o = balance2 s s p 1 * 0.2}
> in audition (out 0 o) {- >> Sound.SC3.UGen.Dot.draw (out 0 o) -}

{var s=SinOsc.ar(440);Out.ar(0,Balance2.ar(s,s,SinOsc.kr(0.2),0.2))}.play
> let {s = sinOsc AR 440 0
>     ;p = sinOsc KR 0.2 0
>     ;o = balance2 s s p 0.2}
> in audition (out 0 o) {- >> Sound.SC3.UGen.Dot.draw (out 0 o) -}

> withSC3 (\fd -> send fd (n_trace [-1]))

{var s=SinOsc.ar(440);Out.ar(0,Balance2.ar(s,s,MouseX.kr(-1,1),0.2))}.play
> let {s0 = sinOsc AR 440 0
>     ;s1 = sinOsc AR 550 0
>     ;x = mouseX KR (-1) 1 Linear 0.2}
> in audition (out 0 (balance2 s0 s0 x 0.2))
