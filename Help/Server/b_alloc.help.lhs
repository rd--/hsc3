    Sound.SC3.Server.Help.viewServerHelp "/b_alloc"

Buffer indices are not restricted by the number of available buffers
at the server.  Below allocates a buffer at index 2 ^ 15.

> import Sound.SC3 {- hsc3 -}

> b0 :: Num n => n
> b0 = 2 ^ 15

> m0 = b_alloc_setn1 b0 0 [0,3,7,10]

    withSC3 (async m0)

> g0 =
>     let x = mouseX KR 0 9 Linear 0.1
>         k = degreeToKey b0 x 12
>     in sinOsc AR (midiCPS (48 + k)) 0 * 0.1
