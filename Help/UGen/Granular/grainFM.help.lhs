grainFM nc tr dur carfreq modfreq index pan envbuf maxgrn

Granular synthesis with frequency modulated sine tones

nc - the number of channels to output. If 1, mono is returned and
     pan is ignored.

tr - a kr or ar trigger to start a new grain. If ar, grains after
     the start of the synth are sample accurate.

The following args are polled at grain creation time

dur - size of the grain.

carfreq - the carrier freq of the grain generators internal
          oscillator

modfreq - the modulating freq of the grain generators internal
          oscillator

index - the index of modulation

pan - a value from -1 to 1. Determines where to pan the output in
      the same manner as PanAz.

envbuf - the buffer number containing a singal to use for the grain
         envelope. -1 uses a built-in Hanning envelope.

> import Sound.SC3.Monadic

> let { d = 15
>     ; lin a b = line KR a b d RemoveSynth
>     ; l = lin (-0.5) 0.5
>     ; f = lin 200 800
>     ; t = impulse KR (lin 7.5 15) 0
>     ; i = lin (-1) 1 }
> in audition (out 0 (grainFM 2 t 0.1 f 200 i l (-1) 512 * 0.1))

> do { n1 <- whiteNoise KR
>    ; n2 <- lfNoise1 KR 500
>    ; let { d = 5
>          ; x = mouseX' KR (-0.5) 0.5 Linear 0.1
>          ; y = mouseY' KR 0 400 Linear 0.1
>          ; f = n1 * y + 440
>          ; t = impulse KR 12.5 0
>          ; i = linLin n2 (-1) 1 1 10 }
>      in audition (out 0 (grainFM 2 t 0.1 f 200 i x (-1) 512 * 0.1)) }
