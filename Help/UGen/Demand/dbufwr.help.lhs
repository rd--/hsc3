> Sound.SC3.UGen.Help.viewSC3Help "Dbufwr"
> Sound.SC3.UGen.DB.ugenSummary "Dbufwr"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.Monad as M {- hsc3 -}

> do {s1 <- M.dseries 30 0 3
>    ;s2 <- M.dseries 30 0 1
>    ;s3 <- M.dseries 16 1 1
>    ;s4 <- M.dwhite 8 1 16
>    ;s5 <- M.dseq dinf (mce2 s3 s4)
>    ;wt <- M.dust KR 1                  {- write trigger -}
>    ;rp <- M.dseries dinf 0 1           {- read pointer -}
>    ;wp <- M.dseq dinf (mce2 s1 s2)     {- write pointer -}
>    ;r <- M.dbufrd 0 rp Loop            {- reader -}
>    ;w <- M.dbufwr 0 wp (s5 * 60) Loop  {- writer -}
>    ;let {d = demand wt 0 w
>         ;f = lag (demand (impulse KR 16 0) 0 r) 0.01
>         ;o = sinOsc AR (f * mce2 1 1.01) 0 * 0.1
>         ;g = mrg [d, out 0 o]
>         ;run = do {_ <- async (b_alloc_setn1 0 0 (replicate 24 210))
>                   ;play g}}
>     in withSC3 run}
