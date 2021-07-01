> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

Terrain function, ie. (x,y) -> z

> ter_f (x,y) =
>     let a = x ** 2
>         b = abs (sin (10 * y)) ** (1/3)
>     in 2 * (a + b) - 1

Create terrain given function.

> gen_t z =
>     let w = 100 {- width -}
>         h = 50 {- height -}
>         tk n = take (round n)
>         xs = tk w [0,1/w ..]
>         ys = tk h [0,1/h ..]
>         ix = map (\y -> map (\x -> (x,y)) xs) ys
>         addz = map (\(x,y) -> (x,y,z (x,y)))
>     in concatMap addz ix

Confirm terrain

    import Sound.SC3.Plot {- hsc3-plot -}
    plot_p3_ln [gen_t ter_f]

Create table.

> tbl' = map (\(_,_,z) -> z) (gen_t ter_f)

Confirm table

    plot_p1_ln [tbl']

Send table to scsynth

    withSC3 (async (b_alloc_setn1 0 0 tbl'))

Hear terrain

> gr_01 =
>     let x' = mouseX kr 1 200 Exponential 0.2
>         y' = mouseY kr 1 300 Exponential 0.2
>         x = abs (sinOsc ar x' 0) + lfNoise2Id 'α' ar 2
>         y = abs (sinOsc ar y' (pi / 2))
>         b = 0
>     in X.waveTerrain ar b x y 100 50

Alternate terrain function.

> ter_f' (x,y) =
>     let a = (cos(5 * x + 1.7)) ** 3
>         b = abs (sin (23 * y)) ** (1/3)
>     in (a - b)

Free buffer

    withSC3 (async (b_free 0))

Gerate mesh given terrain given function.

> gen_mesh z =
>     let w = 100 {- width -}
>         h = 50 {- height -}
>         tk n = take (round n)
>         xs = tk w [0,1/w ..]
>         ys = tk h [0,1/h ..]
>         ix0 = map (\y -> map (\x -> (x,y)) xs) ys
>         ix1 = map (\x -> map (\y -> (x,y)) ys) xs
>         add_z = map (\(x,y) -> (x,y,z (x,y)))
>     in (map add_z ix0,map add_z ix1)

Confirm terrain mesh

    plot_p3_ln ((\(p,q) -> p ++ q) (gen_mesh ter_f))
