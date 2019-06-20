> import Sound.SC3 {- hsc3 -}

fast notes of random duration for 0.5 seconds, then a single note for 0.5 seconds

there is no Dexprand

> dexprand z rp l r = lin_exp (dwhite z rp 0 1) 0 1 l r

> g_01 =
>   let t = dconst 'α' 0.5 (dwhite 'β' dinf 0.05 0.08) 0.001
>       f = duty KR (dseq 'γ' dinf (mce2 t 0.5)) 0 DoNothing (dexprand 'δ' dinf 200 600)
>   in varSaw AR (lag f 0.02) 0 0.3 * 0.1
