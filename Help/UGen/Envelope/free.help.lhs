free trig nodeID

When triggered frees a node.

trig   - when triggered, frees node
nodeID - node to be freed

> withSC3 (\fd -> do let a = out 0 (sinOsc AR 880 0 * 0.1)
>                    send fd (d_recv (graphdef "a" (graph a)))
>                    wait fd "/done"
>                    n0 <- pinkNoise AR
>                    n1 <- dust AR 2
>                    let b = MRG [out 1 (n0 * 0.1), free n1 1001]
>                    send fd (d_recv (graphdef "b" (graph b)))
>                    wait fd "/done"
>                    send fd (s_new "a" 1001 AddToTail 0 [])
>                    send fd (s_new "b" (-1) AddToTail 0 []))
