voscil (rd)

> let { rrand l r = getStdRandom (randomR (l, r))
>     ; lfn r = U.lfNoise0 kr r
>     ; b = 32
>     ; hb = (constant b - 1) / 2
>     ; bn = 8192 * 4
>     ; rt = 6
>     ; f = 600
>     ; s = let { v = vOsc ar (lfn rt * hb + hb) (f * 2) 0
>               ; o = let { bf = lfn rt * 40 + 600
>                         ; nh = lfn rt * 16 + 24 }
>                     in blip ar bf nh * (lfn rt * 0.1 + 0.1)
>               ; p = pan2 (v + o) (lfn rt) (lfn rt * 0.5 + 0.5)
>               ; w = vOsc ar (lfSaw kr (1 / rt) 0 * hb + hb) f 0
>               ; q = pan2 w (lfn rt) (lfn rt * 0.5 + 0.5) }
>           in p + q
>     ; run fd = let r_set i = 
>                        do { m <- rrand 2 512
>                           ; j <- replicateM m (rrand 0 bn)
>                           ; k <- replicateM m (rrand (-1) 1)
>                           ; async fd (b_alloc i bn 1)
>                           ; send fd (b_set i (zip j k)) }
>                in do { mapM_ r_set [0 .. (b - 1)]
>                      ; play fd (out 0 s) } }
> in withSC3 run
