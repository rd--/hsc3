moogFF in freq gain reset

Moog VCF implementation, designed by Federico Fontana. A digital
implementation of the Moog VCF (filter).

in - the input signal
freq - the cutoff frequency
gain - the filter resonance gain, between zero and 4
reset - when greater than zero, this will reset the 
        state of the digital filters at the beginning 
        of a computational block.

The design of this filter is described in the conference paper
Fontana, F. (2007) Preserving the Digital Structure of the Moog
VCF. In Proc. ICMC07, Copenhagen, 25-31 August 2007

> do { n <- whiteNoise AR
>    ; let { y = mouseY KR 100 10000 Exponential 0.1
>          ; x = mouseX KR 0 4 Linear 0.1 }
>      in audition (out 0 (moogFF (n * 0.1) y x 0)) }

> do { n <- lfNoise0 KR 0.43
>    ; let { p = pulse AR (mce [40, 121]) (mce [0.3, 0.7])
>          ; f = linLin (sinOsc KR (linLin n 0 1 0.001 2.2) 0) (-1) 1 30 4200
>          ; y = mouseY KR 1 4 Linear 0.1 }
>      in audition (out 0 (moogFF p f (0.83 * y) 0)) }
