twChoose trig array weights normalize

The output is selected randomly on recieving a trigger from an
array of inputs.  The weights of this choice are determined from
the weights array.  If normalize is set to 1 the weights are
continuously normalized, which means an extra calculation overhead.
When using fixed values the normalizeSum method can be used to
normalize the values.  TWChoose is a composite of TWindex and
Select

> let x = mouseX KR 1 1000 Exponential 0.1
> in do { d <- dust AR x
>       ; let { a = mce [ sinOsc AR 220 0
>                       , saw AR 440
>                       , pulse AR 110 0.1] 
>             ; w = mce [0.5, 0.35, 0.15] }
>         in do { o <- twChoose d a w 0
>               ; audition (out 0 (o * 0.1)) } }

Note: all the ugens are continously running. This may not be the
most efficient way if each input is cpu-expensive.
