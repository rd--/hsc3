free trig nodeID

When triggered frees a node.

trig   - when triggered, frees node
nodeID - node to be freed

> let { a = out 0 (sinOsc AR 880 0 * 0.1) 
>     ; b = do { n0 <- pinkNoise AR
>              ; n1 <- dust AR 2
>              ; return (mrg [out 1 (n0 * 0.1), free n1 1001]) }
>     ; async h m = send h m >> wait h "/done" }
> in withSC3 (\fd -> do { async fd . d_recv . graphdef "a" . graph $ a
>                       ; async fd . d_recv . graphdef "b" . graph =<< b
>                       ; send fd (s_new "a" 1001 AddToTail 0 [])
>                       ; send fd (s_new "b" (-1) AddToTail 0 []) } )
