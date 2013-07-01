> Sound.SC3.UGen.Help.viewSC3Help "EnvDetect"
> Sound.SC3.UGen.DB.ugenSummary "EnvDetect"

> import Sound.SC3.ID

> let {i = soundIn 4
>     ;c = envDetect AR i 0.01 0.1
>     ;f = pitch i 440 60 4000 100 16 1 0.01 0.5 1 0
>     ;o = pinkNoise 'a' AR * c + sinOsc AR f 0 * c}
> in audition (out 0 (mce2 i o))
