> Sound.SC3.UGen.Help.viewSC3Help "Dbufwr"
> Sound.SC3.UGen.DB.ugenSummary "Dbufwr"

> import Sound.SC3 {- hsc3 -}

> do {s1 <- dseriesM 30 0 3
>    ;s2 <- dseriesM 30 0 1
>    ;s3 <- dseriesM 16 1 1
>    ;s4 <- dwhiteM 8 1 16
>    ;s5 <- dseqM dinf (mce2 s3 s4)
>    ;wt <- dustM KR 1                  {- write trigger -}
>    ;rp <- dseriesM dinf 0 1           {- read pointer -}
>    ;wp <- dseqM dinf (mce2 s1 s2)     {- write pointer -}
>    ;r <- dbufrdM 0 rp Loop            {- reader -}
>    ;w <- dbufwrM 0 wp (s5 * 60) Loop  {- writer -}
>    ;let {d = demand wt 0 w
>         ;f = lag (demand (impulse KR 16 0) 0 r) 0.01
>         ;o = sinOsc AR (f * mce2 1 1.01) 0 * 0.1
>         ;g = mrg [d, out 0 o]
>         ;run = do {_ <- async (b_alloc_setn1 0 0 (replicate 24 210))
>                   ;play g}}
>     in withSC3 run}

variant written using a local buffer and uid ugen forms

> let {b = asLocalBuf 'α' (replicate 24 210)
>     ;s = dseq 'β' dinf (mce2 (dseries 'γ' 16 1 1) (dwhite 'δ' 8 1 16))
>     ;rp = dseries 'ε' dinf 0 1 {- read pointer -}
>     ;wp = dseq 'ζ' dinf (mce2 (dseries 'η' 30 0 3) (dseries 'θ' 30 0 1)) {- write pointer -}
>     ;r = dbufrd 'ι' b rp Loop {- reader -}
>     ;w = dbufwr 'κ' b wp (s * 60) Loop {- writer -}
>     ;d = demand (dust 'λ' KR 1) 0 w
>     ;f = lag (demand (impulse KR 16 0) 0 r) 0.01
>     ;o = sinOsc AR (f * mce2 1 1.01) 0 * 0.1
>     ;g = mrg2 d (out 0 o)}
> in audition g

demand rate single memory recurrence relation.

> let rec1 z k t f =
>   let {b = asLocalBuf z [k]
>       ;r = dbufrd z b 0 Loop {- reader -}
>       ;w = dbufwr z b 0 (f r) Loop} {- writer -}
>   in mrg2 (demand t 0 r) (demand t 0 w)

simple counter, written in terms of rec1.

> let {f = rec1 'α' 0 (impulse KR 6 0) (\r -> (r + 1) `modE` 24)
>     ;o = sinOsc AR (midiCPS (60 + f)) 0 * 0.1}
> in audition (out 0 o)
