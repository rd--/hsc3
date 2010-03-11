tgrn (rd)
ghci -e main tgrn.lhs
C-cC-l C-cC-m

> import Sound.SC3.ID

> tgrn b =
>   let { trate = mouseY kr 2 120 Exponential 0.1
>       ; dur = 1.2 / trate
>       ; clk = impulse ar trate 0
>       ; pos = mouseX kr 0 (bufDur kr b) Linear 0.1
>       ; pan = whiteNoise 'α' kr * 0.6
>       ; n = roundE (whiteNoise 'β' kr * 3) 1
>       ; rate = shiftLeft 1.2 n }
>   in tGrains 2 clk b rate pos dur pan 0.25 2

> main =
>   let fn = "/home/rohan/audio/text.snd"
>   in withSC3 (\fd -> do { async fd (b_allocRead 10 fn 0 0)
>                         ; audition (out 0 (tgrn 10)) })
