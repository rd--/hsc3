atan2 x y

Returns the arctangent of y/x.

See also hypot.

Add a pan to the hypot doppler examples by using atan2 to find the
azimuth, or direction angle, of the sound source.  Assume speakers
at +/- 45 degrees and clip the direction to between those.

> let x = 10
>     y = lfSaw KR (1 / 6) 0 * 100
>     d = hypot x y
>     a = 40 / (squared d)
>     s = rlpf (fSinOsc AR 200 0 * lfPulse AR 31.3 0 0.4) 400 0.3
>     z = atan2 y x
>     l = clip2 (z / (pi / 2)) 1
> audition $ pan2 (delayL s (110 / 344) (d / 344)) l a
