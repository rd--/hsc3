> Sound.SC3.UGen.Help.viewSC3Help "WaveTerrain"
> Sound.SC3.UGen.DB.ugenSummary "WaveTerrain"

> import Sound.SC3.ID {- hsc3 -}
> import Sound.SC3.Plot {- hsc3-plot -}

Terrain function

> let z (x,y) = let {a = x ** 2
>                   ;b = abs (sin (10 * y)) ** (1/3)}
> in 2 * (a + b) - 1

Create terrain given function.

> let t z = let {w = 100 {- width -}
>               ;h = 50 {- height -}
>               ;tk n = take (round n)
>               ;ix = [(x,y) | y <- tk h [0,1/h ..], x <- tk w [0,1/w ..]]}
>           in map (\(x,y) -> (x,y,z (x,y))) ix

Confirm terrain

> plot_p3_pt [t z]

Create table.

> let t' = map (\(_,_,z) -> z) (t z)

Confirm table

> plotTable [t']

Send table to scsynth

> withSC3 (async (b_alloc_setn1 0 0 t'))

Hear terrain

> let {x' = mouseX KR 1 200 Exponential 0.2
>     ;y' = mouseY KR 1 300 Exponential 0.2
>     ;x = abs (sinOsc AR x' 0) + lfNoise2 'α' AR 2
>     ;y = abs (sinOsc AR y' (pi / 2))
>     ;b = 0
>     ;o = waveTerrain AR b x y 100 50}
> in audition (out 0 o)

Alternate terrain function.

> let z (x,y) = let {a = (cos(5 * x + 1.7)) ** 3
>                   ;b = abs (sin (23 * y)) ** (1/3)}
>               in (a - b)
