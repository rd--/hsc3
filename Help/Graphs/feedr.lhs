feedr (rd)
warning: input/output feedback loop

> let { delayWr b i = recordBuf b 0 1 0 1 Loop 0 i
>     ; tap nc b dt = playBuf nc b 1 0 (dt * (- sampleRate)) Loop
>     ; dl = 6
>     ; feedr n = do { t <- replicateM n (M.rand 0.0 (constant dl))
>                    ; g <- replicateM n (M.rand 0.4 1.0)
>                    ; f <- replicateM n (M.rand 0.9 0.95)
>                    ; let { d = zipWith (\p q -> tap 2 10 p * q) t g
>                          ; x = mouseX kr 0.02 1.0 Exponential 0.1
>                          ; s = clip2 (leakDC (hpf (sum d) 20) 0.995) 1
>                          ; i = soundIn (mce2 0 1)
>                          ; r = i + sum (map (* x) (zipWith (*) d f)) }
>                      in return (mrg [out 0 s, delayWr 10 r]) } }
> in withSC3 (\fd -> do { nf <- fmap (* dl) (serverSampleRateActual fd)
>                       ; send fd (b_alloc 10 (floor nf) 2)
>                       ; audition =<< feedr 18 })

> withSC3 (\fd -> send fd (b_zero 10))
