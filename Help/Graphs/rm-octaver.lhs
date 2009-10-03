rm-octaver (andrea valle, miller puckette)

> import Sound.SC3

> main =
>   let { defaultPitch x = pitch x 440 60 4000 100 16 1 0.01 0.5 1
>       ; i = soundIn 0
>       ; p = defaultPitch i
>       ; f = mceChannel 0 p }
>   in audition (out 0 (sinOsc ar (f * 0.5) 0 * i + i))
