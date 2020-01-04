> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

> f_01 (b,l) = do
>  let z = [Normalise,Wavetable,Clear]
>  _ <- async (b_alloc b 512 1)
>  sendMessage (b_gen_sine1 b z (map recip l))

> t_01 :: Transport m => m ()
> t_01 = do
>   let a = [[13,8,55,34,5,21,3,1,2],[55,34,1,3,2,13,5,8,21]]
>   mapM_ f_01 (zip [10,11] a)

    withSC3 t_01

> g_01 = lfSaw AR (mce2 110 164) 0 * 0.1 * osc1 AR (mce2 10 11) 4 RemoveSynth

    audition g_01
