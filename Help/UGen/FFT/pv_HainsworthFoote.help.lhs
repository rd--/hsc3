pv_HainsworthFoote buf proph propf threshold waittime

FFT onset detector based on work described in

  Hainsworth, S. (2003) Techniques for the Automated Analysis of
  Musical Audio. PhD, University of Cambridge engineering dept. See
  especially p128.

The Hainsworth metric is a modification of the Kullback Liebler
distance.

The onset detector has general ability to spot spectral change, so may
have some ability to track chord changes aside from obvious transient
jolts, but there's no guarantee it won't be confused by frequency
modulation artifacts.

Hainsworth metric on it's own gives good results but Foote might be
useful in some situations: experimental.

    buffer - FFT buffer to read from

     proph - What strength of detection signal from
             Hainsworth metric to use.

     propf - What strength of detection signal from Foote
             metric to use. The Foote metric is normalised
             to [0.0,1.0]

 threshold - Threshold hold level for allowing a detection

  waittime - If triggered, minimum wait until a further
             frame can cause another spot (useful to stop
             multiple detects on heavy signals)

> import Sound.SC3

> let { i = soundIn 0
>     ; b = mrg2 (localBuf 'a' 2048 1) (maxLocalBufs 1)
>     ; f = fft' b i
>     ; x = mouseX KR 0.5 1.25 Linear 0.2
>     ; h = pv_HainsworthFoote f 1 0 x 0.04
>     ; o = sinOsc AR (mrg2 440 445) 0 * decay (h * 0.1) 0.1 }
> in audition (out 0 (o + i))
