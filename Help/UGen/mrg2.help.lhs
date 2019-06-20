> import Sound.SC3 {- hsc3 -}

mrg2 defines a node indicating a multiple root graph.

> g_01 =
>   let l = out 0 (sinOsc AR 300 0 * 0.1)
>       r = out 1 (sinOsc AR 900 0 * 0.1)
>   in mrg2 l r

there is a leftmost rule, so that mrg nodes need not
be terminal.

> g_02 =
>   let l = sinOsc AR 300 0 * 0.1
>       r = out 1 (sinOsc AR 900 0 * 0.1)
>   in mrg2 l r

the leftmost node may be an mce node

> g_03 =
>   let l = sinOsc AR (mce2 300 400) 0 * 0.1
>       r = out 1 (sinOsc AR 900 0 * 0.1)
>   in mrg2 l r

the implementation is not thorough

> g_04 =
>   let l = sinOsc AR (mce2 300 400) 0 * 0.1
>       r = out 1 (sinOsc AR 900 0 * 0.1)
>   in out 0 (mrg2 l r + mrg2 l r)
