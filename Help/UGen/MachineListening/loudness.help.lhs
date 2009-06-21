loudness chain smask tmask

Extraction of instantaneous loudness in sones.

 chain [fft] - Audio input to track, which has been pre-analysed by
               the FFT UGen; see examples below for the expected FFT
               size

  smask [sk] - Spectral masking param: lower bins mask higher bin
               power within ERB bands, with a power falloff (leaky
               integration multiplier) of smask per bin. (=0.25)

  tmask [sk] - Temporal masking param: the phon level let through in
               an ERB band is the maximum of the new measurement, and
               the previous minus tmask phons (=6)

A perceptual loudness function which outputs loudness in sones; this
is a variant of an MP3 perceptual model, summing excitation in ERB
bands. It models simple spectral and temporal masking, with equal
loudness contour correction in ERB bands to obtain phons (relative
dB), then a phon to sone transform. The final output is typically in
the range of 0 to 64 sones, though higher values can occur with
specific synthesised stimuli

 given a sinOsc at 1000hz: gain 0.001 => loudness 1 sone
                           gain 0.010 => loudness 4 sone
                           gain 0.100 => loudness 16 sone
                           gain 1.000 => loudness 64 sone

Assume hop of half fftsize.

> import Sound.SC3

> let { b = 10
>     ; x = mouseX KR 0.001 0.1 Exponential 0.2
>     ; i = sinOsc AR 1000 0 * x
>     ; f = fft' (constant b) i
>     ; l = loudness f 0.25 6
>     ; o = sinOsc AR (mce2 900 (l * 300 + 600)) 0 * 0.1 }
> in withSC3 (\fd -> do { async fd (b_alloc b 1024 1)
>                       ; audition (out 0 o) })

Research note: This UGen is an informal juxtaposition of perceptual
coding, and a Zwicker and Glasberg/Moore/Stone loudness model.
