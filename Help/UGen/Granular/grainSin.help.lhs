grainSin nc tr dur freq pan envbuf maxgrn

Granular synthesis with sine tones

nc - the number of channels to output. If 1, mono is returned and
     pan is ignored.

tr - a kr or ar trigger to start a new grain. If ar, grains after
     the start of the synth are sample accurate.

The following args are polled at grain creation time

dur - size of the grain.

freq - the input to granulate

pan - a value from -1 to 1. Determines where to pan the output in
      the same manner as PanAz.

envbuf - the buffer number containing a singal to use for the grain
         envelope. -1 uses a built-in Hanning envelope.

> import Sound.SC3.Monadic

> do { n <- whiteNoise KR
>    ; let { x = mouseX KR (-0.5) 0.5 Linear 0.1
>          ; y = mouseY KR 0 400 Linear 0.1
>          ; f = n * y + 440
>          ; t = impulse KR 10 0 }
>      in audition (out 0 (grainSin 2 t 0.1 f x (-1) 512 * 0.1)) }
