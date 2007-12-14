grainBuf nc tr dur sndbuf rate pos interp pan envbuf

Granular synthesis with sound stored in a buffer

nc - the number of channels to output. If 1, mono is returned and
     pan is ignored.

tr - a kr or ar trigger to start a new grain. If ar, grains after
     the start of the synth are sample accurate.

The following args are polled at grain creation time

dur - size of the grain.

sndbuf - the buffer holding an audio signal

rate - the playback rate of the sampled sound

pos - the playback position for the grain to start with (0 is
      beginning, 1 is end of file)

interp - the interpolation method used for pitchshifting grains.
         1 = no interpolation. 2 = linear. 4 = cubic interpolation
         (more computationally intensive).

pan - a value from -1 to 1. Determines where to pan the output in
      the same manner as PanAz.

envb - the buffer number containing a singal to use for the
       grain envelope. -1 uses a built-in Hanning envelope.

> withSC3 (\fd -> send fd (b_allocRead 10 "/home/rohan/audio/metal.wav" 0 0))
> n1 <- lfNoise1 KR 500
> n2 <- lfNoise2 KR 0.1
> let b = 10
>     e = -1
>     x = mouseX KR (-1) 1 Linear 0.1
>     y = mouseY KR 10 45 Linear 0.1
>     i = impulse KR y 0
>     r = linLin n1 (-1) 1 0.5 2
>     p = linLin n2 (-1) 1 0 1
> audition (out 0 (grainBuf 2 i 0.1 b r p 2 x e))
