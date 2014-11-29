> Sound.SC3.UGen.Help.viewSC3Help "WaveTerrain"
> Sound.SC3.UGen.DB.ugenSummary "WaveTerrain"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.Plot {- hsc3-plot -}

Terrain function

> let z (x,y) = let {a = x ** 2
>                   ;b = abs (sin (10 * y)) ** (1/3)}
> in 2 * (a + b) - 1

Create terrain given function.

> let t z = let {w = 100 {- width -}
>               ;h = 50 {- height -}
>               ;tk n = take (round n)
>               ;xs = tk w [0,1/w ..]
>               ;ys = tk h [0,1/h ..]
>               ;ix = map (\y -> map (\x -> (x,y)) xs) ys
>               ;add_z = map (\(x,y) -> (x,y,z (x,y)))}
>           in concatMap add_z ix

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

Free buffer

> withSC3 (async (b_free 0))

Gerate mesh given terrain given function.

> let t z = let {w = 100 {- width -}
>               ;h = 50 {- height -}
>               ;tk n = take (round n)
>               ;xs = tk w [0,1/w ..]
>               ;ys = tk h [0,1/h ..]
>               ;ix0 = map (\y -> map (\x -> (x,y)) xs) ys
>               ;ix1 = map (\x -> map (\y -> (x,y)) ys) xs
>               ;add_z = map (\(x,y) -> (x,y,z (x,y)))}
>           in (map add_z ix0,map add_z ix1)

Confirm terrain mesh

> plot_p3_ln ((\(p,q) -> p ++ q) (t z))
