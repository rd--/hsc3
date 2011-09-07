> Sound.SC3.UGen.Help.viewSC3Help "Operator.=="
> :t (==*)

#hsc3
The star suffixes (<*,<=*,>*,>=*) are because the result of the
operatros is not of type Bool, as is required by the signature for the
class Ord.

> import Sound.SC3

> let { o = sinOsc KR 1 0
>     ; t = [o >* 0
>           ,o >=* 0
>           ,o <* 0
>           ,o <=* 0
>           ,o ==* 0
>           ,(o <* 0.001) * (o >* (-0.001))]
>     ; f = [220, 330, 440, 550, 660, 770]
>     ; p = envPerc 0.01 1
>     ; e = envGen KR (mce t) 0.1 0 1 DoNothing p }
> in audition (out 0 (mix (sinOsc AR (mce f) 0 * e)))
