Note: SC3 reorders the mul and add inputs to precede the doneAction input.

> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let f = line KR 200 17000 5 RemoveSynth
>   in sinOsc AR f 0 * 0.1

Demonstrate RemoveGroup done-action.

    > import Sound.OSC {- hosc -}
    > withSC3 (sendMessage (g_new [(10,AddToTail,1)]))

> g_02 =
>    let f = line KR 200 (mce2 209 211) 5 RemoveGroup
>    in sinOsc AR f 0 * 0.1

    > audition_at (-1,AddToTail,10,[]) (out 0 g_02)
