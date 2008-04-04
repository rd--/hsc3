atsSynth b nPartials partialStart partialSkip ptr freqMul freqAdd

Resynthesize sine data from an ATS analysis file.

             b - buffer containing ATS data
     nPartials - number of partials to synthesize
  partialStart - partial to start the synthesis at (zero indexed)
   partialSkip - increment indicating partials to synthesize
           ptr - data index (0, 1)
       freqMul - multiplier for sinusoidal frequency data
       freqAdd - value to add to frequency data

> let { load_data fd b i d = 
>       if length d < 512 
>       then send fd (b_setn1 b i d) 
>       else do { send fd (b_setn1 b i (take 512 d))
>               ; load_data fd b (i + 512) (drop 512 d) } }
> in withSC3 (\fd -> do { ats <- atsRead "/home/rohan/tn/tn-56/ats/metal.ats"
>                       ; let { d = atsSC3 ats
>                             ; h = atsHeader ats
>                             ; x = mouseX KR 0.05 1.5 Linear 0.2
>                             ; y = mouseY KR 0.25 2.0 Linear 0.2
>                             ; np = constant (atsNPartials h)
>                             ; f = x / constant (atsAnalysisDuration h)
>                             ; ptr = lfSaw AR f 1 * 0.5 + 0.5
>                             ; rs = atsSynth 10 np 0 1 (clip ptr 0 1) y 0 }
>                         in do { async fd (b_alloc 10 (length d) 1)
>                               ; load_data fd 10 0 d
>                               ; play fd (out 0 rs) } })
