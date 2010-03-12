free trig nodeID

When triggered frees a node.

trig   - when triggered, frees node
nodeID - node to be freed

> import Sound.SC3.Monadic

> let { a = out 0 (sinOsc AR 880 0 * 0.1) 
>     ; b = do { n0 <- pinkNoise AR
>              ; n1 <- dust AR 20
>              ; return (mrg [out 1 (n0 * 0.1), free n1 1001]) } }
> in withSC3 (\fd -> do { async fd (d_recv (synthdef "a" a))
>                       ; async fd . d_recv . synthdef "b" =<< b
>                       ; send fd (s_new "a" 1001 AddToTail 0 [])
>                       ; send fd (s_new "b" (-1) AddToTail 0 []) } )
