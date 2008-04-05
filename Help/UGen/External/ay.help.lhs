ay tonea toneb tonec noise ctl vola volb volc envfreq envstyle chiptype

Emulates the General Instrument AY-3-8910 (a.k.a. the Yamaha
YM2149) 3-voice sound chip, as found in the ZX Spectrum
128, the Atari ST, and various other home computers during
the 1980s.

The inputs are as follows:

 * tonea, toneb, tonec - integer "tone" value for each of
   the 3 voices, from 0 to 4095 (i.e. 12-bit range). Higher
   value = lower pitch. For convenience, the AY.freqtotone
   method converts a frequency value to something
   appropriate for these inputs.

 * noise - the period of the pseudo-random noise generator, 
   0 to 31

 * control - controls how the noise is mixed into the
   tone(s), 0 to 32 (0 is mute). This is a binary mask value
   which masks the noise/tone mixture in each channel, so
   it's not linear.

 * vola, volb, volc - volume of the three channels, 0 to 15
   (or 0 to 31 if using YM chiptype)

 * envfreq - envelope frequency, 0 to 4095

 * envstyle - type of envelope used, 0 to 15

 * chiptype - 0 for AY (default), 1 for YM. The models
   behave slightly differently. This input cannot be
   modulated - its value is only handled at the moment the
   UGen starts.

The chip's inputs are integer values, so non-integer values
will be rounded off.

The emulation is provided by the libayemu library:
http://sourceforge.net/projects/libayemu.

> audition (out 0 (ay 1777 1666 1555 1 7 15 15 15 4 1 0))

> let { tonea = mouseY KR 10 3900 Exponential 0.2
>     ; toneb = mouseX KR 10 3900 Exponential 0.2
>     ; ctl = 3
>     ; vola = 14
>     ; volb = 14
>     ; volc = 0 
>     ; s = ay tonea toneb 1555 1 ctl vola volb volc 4 1 0 }
> in audition (out 0 (pan2 s 0 0.25))

> let { rate = mouseX KR 0.1 10 Linear 0.2
>     ; rng l r i = return (linLin i (-1) 1 l r)
>     ; mk_ctl l r = lfdNoise3 KR rate >>= rng l r
>     ; mk_ctl_0 l r = lfdNoise0 KR rate >>= rng l r }
> in do { tonea <- mk_ctl 10 3900
>       ; toneb <- mk_ctl 10 3900
>       ; tonec <- mk_ctl 10 3900
>       ; n <- mk_ctl 0 31
>       ; ctl <- mk_ctl_0 0 31
>       ; vola <- mk_ctl 0 15
>       ; volb <- mk_ctl 0 15
>       ; volc <- mk_ctl 0 15
>       ; envfreq <- mk_ctl 0 4095
>       ; envstyle <- mk_ctl 0 15
>       ; let s = ay tonea toneb tonec n ctl vola volb volc envfreq envstyle 0
>         in audition (out 0 (pan2 s 0 0.5)) }
