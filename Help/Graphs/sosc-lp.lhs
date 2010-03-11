sosc-lp (rd)

> import Sound.SC3.Monadic

> main =
>   let { dustR r lo hi = do { n1 <- dwhite 1 lo hi
>                            ; n2 <- whiteNoise r
>                            ; d <- dseq dinf n1
>                            ; return (tDuty r d 0 DoNothing (abs n2) 1) }
>       ; a = [60, 71, 89, 65, 36, 57, 92, 97, 92, 97]
>       ; b = [71, 89, 60, 57, 65, 36, 95, 92, 93, 97]
>       ; setup fd = do { async fd (b_alloc 10 9 1)
>                       ; async fd (b_alloc 11 9 1)
>                       ; send fd (b_setn1 10 0 a)
>                       ; send fd (b_setn1 11 0 b) }
>       ; d_env t = decay2 t 0.002 2.5
>       ; idx t = stepper t 0 0 15 1 0
>       ; f1 t = let { l = (bufRdL 1 kr 10 (idx t) Loop - 24)
>                    ; r = (bufRdL 1 kr 11 (idx t) Loop - 24) }
>                in midiCPS (mce2 l r)
>       ; f2 t n = f1 t + n * 1.2
>       ; o1 t = sinOsc ar (f1 t) 0 * d_env t
>       ; o2 t n = sinOsc ar (f2 t n) 0 * d_env t
>       ; sosc_lp t n = out 0 ((o1 t + o2 t n) * 0.2) }
>   in do { clk <- dustR kr 0.2 0.9
>         ; n <- lfNoise0 kr (mce2 1 3)
>         ; audition (sosc_lp clk n) }

> resetter =
>   let { a = [71, 60, 65, 89, 36, 57, 95, 97, 92, 97]
>       ; b = [89, 71, 60, 65, 57, 36, 92, 95, 93, 97]
>       ; resetup fd = do { send fd (b_setn1 10 0 a)
>                         ; send fd (b_setn1 11 0 b) } }
>   in withSC3 resetup
