-- equal_to
let o = sinOsc kr 1 0
    t = [o `greater_than` 0
        ,o `greater_than_or_equal_to` 0
        ,o `less_than` 0
        ,o `less_than_or_equal_to` 0
        ,o `equal_to` 0
        ,(o `less_than` 0.001) * (o `greater_than` (-0.001))]
    f = [220, 330, 440, 550, 660, 770]
    p = envPerc 0.01 1
    e = envGen kr (mce t) 0.1 0 1 DoNothing p
in mix (sinOsc ar (mce f) 0 * e)

-- equal_to ; operators
let o = sinOsc kr 1 0
    t = [o >** 0
        ,o >=** 0
        ,o <** 0
        ,o <=** 0
        ,o ==** 0
        ,(o <** 0.001) * (o >** (-0.001))]
    f = [220, 330, 440, 550, 660, 770]
    p = envPerc 0.01 1
    e = envGen kr (mce t) 0.1 0 1 DoNothing p
in mix (sinOsc ar (mce f) 0 * e)

{---- ; operators

The star suffixes (==**,/=**,<**,<=**,>**,>=**) are because the result of the
operators is not of type Bool, as is required by the signature for the
classes Eq and Ord.

-}
