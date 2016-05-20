    > Sound.SC3.UGen.Help.viewSC3Help "Operator.=="
    > :t (==*)

The star suffixes (<*,<=*,>*,>=*) are because the result of the
operators is not of type Bool, as is required by the signature for the
class Ord.  At some point `<*` was written into the standard prelude...

> import Prelude hiding ((<*)) {- base -}
> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let o = sinOsc KR 1 0
>         t = [o >* 0
>             ,o >=* 0
>             ,o <* 0
>             ,o <=* 0
>             ,o ==* 0
>             ,(o <* 0.001) * (o >* (-0.001))]
>         f = [220, 330, 440, 550, 660, 770]
>         p = envPerc 0.01 1
>         e = envGen KR (mce t) 0.1 0 1 DoNothing p
>     in mix (sinOsc AR (mce f) 0 * e)
