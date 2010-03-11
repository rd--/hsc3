karplus strong (alex mclean)

> import Sound.SC3.Monadic

> main =
>   let { aA = ( "aA" 
>              , [800, 1150, 2800, 3500, 4950]
>              , [0, -4  , -20, -36 , -60]
>              , [80, 90,  120, 130, 140] )
>       ; aU = ( "aU" 
>              , [325, 700, 2530, 3500, 4950]
>              , [0, -12 , -30, -40, -64]
>              , [50, 60,  170, 180, 200] )
>       ; cs (_, c1, c2, c3) = c1 ++ c2 ++ c3
>       ; vf i s = let { f = in' 5 kr i
>                      ; a = in' 5 kr (i + 5)
>                      ; b = in' 5 kr (i + 10) }
>                  in mix (resonz s f (b / f) * dbAmp a)
>       ; ks n d = let { x = mouseX kr 0 0.01 Linear 0.1 {- delay -}
>                      ; y = mouseY kr 0.85 1 Linear 0.1 {- blend / gain -}
>                      ; ugenIf a b c = (a * b) + ((1 - a) * c)
>                      ; n0 = (n / 2) + 0.5
>                      ; probSwitch i prob = ugenIf (n0 >* prob) i (negate i)
>                      ; laggedDelay = lag x 0.01
>                      ; o = sinOsc ar 200 0
>                      ; a0 = decay d 0.025 * o
>                      ; a1 = localIn 1 ar + (a0 * (y - 0.25))
>                      ; a2 = delayN a1 0.01 laggedDelay
>                      ; a3 = delay1 a2
>                      ; a4 = (a2 + a3) / 2.0
>                      ; a5 = probSwitch a4 y
>                      ; a6 = vf (toggleFF d * 15) a5
>                      ; a7 = a6 * 1.5 }
>                 in mrg [localOut (a5 * 0.99), out 0 (mce [a7, a7])] }
>   in withSC3 (\fd -> do { send fd (c_setn [(0,cs aA), (15, cs aU)])
>                         ; n <- whiteNoise ar
>                         ; d <- dust kr 4
>                         ; play fd (ks n d) })

Variant on http://doc.gold.ac.uk/~ma503am/alex/vocable-source-released/
