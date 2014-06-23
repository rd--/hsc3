> import Sound.SC3

> withSC3 (let {z = [Normalise,Wavetable,Clear]
>              ;a = [[13,8,55,34,5,21,3,1,2],[55,34,1,3,2,13,5,8,21]]
>              ;f (b,l) = do {_ <- async (b_alloc b 512 1)
>                            ;send (b_gen_sine1 b z (map recip l))}}
>          in mapM_ f (zip [10,11] a))

> audition (out 0 (lfSaw AR (mce2 110 164) 0 * 0.1 * osc1 AR (mce2 10 11) 4 RemoveSynth))
