mouse clatter (rd)

> import Sound.SC3

> let { x = mouseX kr 100 12000 Linear 0.1
>     ; y = mouseY kr 0.01 0.15 Linear 0.1 }
> in do { n1 <- M.lfNoise0 kr (mce [3, 3.25])
>       ; let { t = impulse kr (n1 * 16 + 18) 0
>             ; s = do { n2 <- M.tRand 0.005 y t
>                      ; n3 <- M.whiteNoise ar
>                      ; n4 <- M.tRand 10 x t
>                      ; n5 <- M.tRand 0 1 t
>                      ; n6 <- M.tExpRand 0.15 1 t
>                      ; o <- let e = decay2 t 0.01 n2
>                             in return (bpf (n3 * e) n4 n5)
>                      ; n7 <- M.pv_RandComb (fft' 10 o) n6 t
>                      ; return (o * 0.05 + ifft' n7) } }
>         in withSC3 (\fd -> do { async fd (b_alloc 10 2048 1)
>                               ; play fd . out 0 =<< s }) }

(let* ((x (MouseX kr 100 12000 0 0.1))
       (y (MouseY kr 0.01 0.15 0 0.1))
       (n1 (LFNoise0 kr (mce2 3 3.25)))
       (t (Impulse kr (MulAdd n1 16 18) 0))
       (n2 (TRand 0.005 y t))
       (n3 (WhiteNoise ar))
       (n4 (TRand 10 x t))
       (n5 (TRand 0.0 1.0 t))
       (n6 (TExpRand 0.15 1.0 t))
       (e (Decay2 t 0.01 n2))
       (o (BPF (Mul n3 e) n4 n5))
       (n7 (PV_RandComb (FFT* 10 o) n6 t))
       (s (Add (Mul o 0.05) (IFFT* n7))))
  (with-sc3
   (lambda (fd)
     (async fd (/b_alloc 10 2048 1))
     (play fd (Out 0 s)))))
