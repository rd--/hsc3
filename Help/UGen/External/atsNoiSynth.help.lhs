atsNoiSynth b nPartials partialStart partialSkip ptr sinP noiseP 
            freqMul freqAdd nBands bandStart bandSkip

Resynthesize sine data from an ATS analysis file

           b - buffer containing ATS data
   nPartials - number of partials to synthesize
partialStart - partial in the analysis to start the 
               synthesis on (zero indexed)
 partialSkip - increment indicating partials to synthesize
         ptr - index into data set (0, 1)
       sineP - scaler on sinusoidal portion of the resynthesis
      noiseP - scaler on noise portion of the resynthesis
     freqMul - multiplier on the sinusoidal frequency information.
     freqAdd - value to add to frequency information.
      nBands - number of critical bands (noise) to synthesize.  
               There are 25 critical bands.
   bandStart - critical band to start resynthesis on. 
               0 is the first band.
    bandSkip - increment indicating bands to synthesize.

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
>                             ; ptr = clip (lfSaw AR f 1 * 0.5 + 0.5) 0 1
>                             ; rs = atsNoiSynth 10 np 0 1 ptr 1 1 y 0 25 0 1 }
>                         in do { async fd (b_alloc 10 (length d) 1)
>                               ; load_data fd 10 0 d
>                               ; play fd (out 0 rs) } })

(note: freqMul is not observed)
