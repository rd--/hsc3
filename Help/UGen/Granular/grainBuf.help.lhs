grainBuf nc tr dur sndbuf rate pos interp pan envbuf maxgrn

Granular synthesis with sound stored in a buffer

nc - the number of channels to output. If 1, mono is returned and
     pan is ignored.

tr - a kr or ar trigger to start a new grain. If ar, grains after
     the start of the synth are sample accurate.

The following args are polled at grain creation time

dur - size of the grain (in seconds).

sndbuf - the buffer holding an audio signal (must be single channel)

rate - the playback rate of the sampled sound

pos - the playback position for the grain to start with (0 is
      beginning, 1 is end of file)

interp - the interpolation method used for pitchshifting grains.
         1 = no interpolation. 2 = linear. 4 = cubic interpolation
         (more computationally intensive).

pan - a value from -1 to 1. Determines where to pan the output in
      the same manner as PanAz.

envb - the buffer number containing a signal to use for the
       grain envelope. -1 uses a built-in Hanning envelope.

maxgrn - maxiumum number of grains

> import Sound.SC3.Monadic

> let fn = "/home/rohan/audio/metal.wav"
> in withSC3 (\fd -> send fd (b_allocRead 10 fn 0 0))

> let { buf = 10
>     ; dur = 15
>     ; lin a b = line KR a b dur RemoveSynth
>     ; tr = impulse KR (lin 7.5 15) 0
>     ; gd = lin 0.05 0.1
>     ; r = lin 1 0.5
>     ; i = lin 0 1
>     ; l = lin (-0.5) 0.5
>     ; g = grainBuf 2 tr gd buf r i 2 0 (-1) 512 }
> in audition (out 0 g)

> let { b = 10
>     ; e = -1
>     ; x = mouseX KR (-1) 1 Linear 0.1
>     ; y = mouseY KR 10 45 Linear 0.1
>     ; i = impulse KR y 0
>     ; r n = linLin n (-1) 1 0.5 2
>     ; p n = linLin n (-1) 1 0 1
>     ; g n1 n2 = grainBuf 2 i 0.1 b (r n1) (p n2) 2 x e 512 }
> in withSC3 (\fd -> do { n1 <- lfNoise1 KR 500
>                       ; n2 <- lfNoise2 KR 0.1
>                       ; play fd (out 0 (g n1 n2)) })
