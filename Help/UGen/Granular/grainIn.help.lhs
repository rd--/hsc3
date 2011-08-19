grainIn nc=1 tr=0 dur=1 in pan=0 envbuf=-1 mx=512

Granulate an input signal

nc - the number of channels to output. If 1, mono is
     returned and pan is ignored.
tr - a kr or ar trigger to start a new grain. If ar, grains
     after the start of the synth are sample accurate.

The following args are polled at grain creation time

   dur - size of the grain.
    in - the input to granulate
   pan - a value from -1 to 1. Determines where to pan the output in
         the same manner as PanAz.
envbuf - the buffer number containing a singal to use for the
         grain envelope. -1 uses a built-in Hanning envelope.
    mx - maxiumun number of grains

> import Sound.SC3.ID

> let { n = pinkNoise 'a' AR
>     ; x = mouseX' KR (-0.5) 0.5 Linear 0.1
>     ; y = mouseY' KR 5 25 Linear 0.1
>     ; t = impulse KR y 0
>     ; g = grainIn 2 t 0.1 n x (-1) 512 * 0.1 }
> in audition (out 0 g)
