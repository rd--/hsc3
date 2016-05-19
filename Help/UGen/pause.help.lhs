    > Sound.SC3.UGen.Help.viewSC3Help "Pause"
    > Sound.SC3.UGen.DB.ugenSummary "Pause"

> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let f = control KR "f" 440
>         g = control KR "g" 1
>     in mrg [sinOsc AR f 0 * 0.1,pause g 1001]

    > audition_at (1001,AddToTail,1,[]) g_01
    > audition_at (1002,AddToTail,1,[("f",880)]) g_01

Request that node 1002 pause node 1001.

    > withSC3 (send (n_set 1002 [("g",0)]))

Restart node 1001.

    > withSC3 (send (n_set 1002 [("g",1)]))
