> Sound.SC3.UGen.Help.viewSC3Help "DynKlank"
> Sound.SC3.UGen.DB.ugenSummary "DynKlank"

> import Sound.SC3.ID

{s=`[[800,1071,1153,1723],nil,[1,1,1,1]]
;DynKlank.ar(,Impulse.ar(2,0,0.1))}.play
> let s = klankSpec [800,1071,1153,1723] [1,1,1,1] [1,1,1,1]
> in audition (out 0 (dynKlank (impulse AR 2 0 * 0.1) 1 0 1 s))

{s=`[[800,1071,1353,1723],nil,[1,1,1,1]]
;DynKlank.ar(s,Dust.ar(8,0.1))}.play
> let s = klankSpec [800,1071,1353,1723] [1,1,1,1] [1,1,1,1]
> in audition (out 0 (dynKlank (dust 'a' AR 8 * 0.1) 1 0 1 s))

{s=`[[800,1071,1353,1723],nil,[1,1,1,1]]
;DynKlank.ar(s,PinkNoise.ar(0.007))}.play
> let s = klankSpec [800,1071,1353,1723] [1,1,1,1] [1,1,1,1]
> in audition (out 0 (dynKlank (pinkNoise 'a' AR * 0.007) 1 0 1 s))

{s=`[[200,671,1153,1723],nil,[1,1,1,1]]
;a=[0.007,0.007]
;DynKlank.ar(s,PinkNoise.ar(a))}.play;
> let {s = klankSpec [200,671,1153,1723] [1,1,1,1] [1,1,1,1]
>     ;a = mce2 0.007 0.007}
> in audition (out 0 (dynKlank (pinkNoise 'a' AR * a) 1 0 1 s))

change freqs and ringtimes with mouse
> let {x = mouseX KR 0.5 2 Exponential 0.2
>     ;f = map (* x) [800,1071,1153,1723]
>     ;y = mouseY KR 0.1 10 Exponential 0.2
>     ;d = map (* y) [1,1,1,1]
>     ;s = klankSpec f [1,1,1,1] d
>     ;i = impulse AR 2 0 * 0.1}
> in audition (out 0 (dynKlank i 1 0 1 s))
