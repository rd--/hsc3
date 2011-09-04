dfm1 in freq res inputgain type noiselevel

Digital Filter Module.  dfm1 is a digitally modelled analog filter.
It provides low-pass and high-pass filtering.  The filter can be
overdriven and will self-oscillate at high resonances.

         in - input signal
       freq - cutoff frequency (1000.0)
        res - resonance (0.1)
  inputgain - gain applied to the input signal (1.0)
       type - set to 0.0 for low-pass or 1.0 for high-pass (0.0)
 noiselevel - amplitude of noise added to the model (0.0003) (3e-4)

> import Sound.SC3.ID

Play it with the mouse
> let { n = pinkNoise 'a' AR * 0.5
>     ; x = mouseX' KR 80 5000 Exponential 0.1
>     ; y = mouseX' KR 0.1 1.2 Linear 0.1 }
> in audition (out 0 (dfm1 n x y 1 0 3e-4))

Bass
> let { i = pulse AR 100 0.5 * 0.4 + pulse AR 100.1 0.5 * 0.4
>     ; f = range 80 2000 (sinOsc KR (range 0.2 5 (sinOsc KR 0.3 0)) 0)
>     ; s = dfm1 i f 1.1 2 0 3e-4 }
> in audition (out 0 (mce2 s s))
