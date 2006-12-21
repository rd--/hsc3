pause gate nodeID

When triggered pauses a node.

gate   - when gate is 0,  node is paused, when 1 it runs
nodeID - node to be paused

> let f  = Control KR "f" 440
>     g  = Control KR "g" 1
>     a  = MRG [out 0 (sinOsc AR f 0 * 0.1), pause g 1001]
>     a' = graphdef "a" (graph a)
> withSC3 (\fd -> do send fd (d_recv a')
>                    wait fd "/done"
>                    send fd (s_new "a" 1001 AddToTail 0 [])
>                    send fd (s_new "a" 1002 AddToTail 0 [("f", 880)]))

Request that node 1002 pause node 1001.

> withSC3 (\fd -> send fd (n_set 1002 [("g", 0)]))

Restart node 1001.

> withSC3 (\fd -> send fd (n_set 1002 [("g", 1)]))
