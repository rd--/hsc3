> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> g_01 =
>     let i = soundIn 0
>         c = X.envDetect AR i 0.01 0.1
>         p = pitch i 440 60 4000 100 16 1 0.01 0.5 1 0
>         f = mceChannel 0 p * 3
>         e = lagUD (mceChannel 1 p) 0 0.1
>         o = pinkNoise 'α' AR * c + sinOsc AR f 0 * c * e
>     in mce2 i o
