rotate2 x y pos

Rotate a sound field.  Rotate2 can be used for rotating an
ambisonic B-format sound field around an axis.  Rotate2 does an
equal power rotation so it also works well on stereo sounds.  It
takes two audio inputs (x, y) and an angle control (pos).  It
outputs two channels (x, y).

It computes:

     xout = cos(angle) * xin + sin(angle) * yin
     yout = cos(angle) * yin - sin(angle) * xin

where angle = pos * pi, so that -1 becomes -pi and +1 becomes +pi.
This allows you to use an LFSaw to do continuous rotation around a
circle.

The control pos is the angle to rotate around the circle from -1
to +1. -1 is 180 degrees, -0.5 is left, 0 is forward, +0.5 is
right, +1 is behind.

Rotation of stereo sound, via LFO.

> import Sound.SC3.ID

> let { x = pinkNoise 'a' AR
>     ; y = lfTri AR 800 0 * lfPulse KR 3 0 0.3 * 0.2 }
> in audition (out 0 (rotate2 x y (lfSaw KR 0.1 0)))

Rotation of stereo sound, via mouse.

> let { x = mix (lfSaw AR (mce [198..201]) 0 * 0.1)
>     ; y = sinOsc AR 900 0 * lfPulse KR 3 0 0.3 * 0.2 
>     ; p = mouseX KR 0 2 Linear 0.2 }
> in audition (out 0 (rotate2 x y p))
