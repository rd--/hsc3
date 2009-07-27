warp1 nc buf ptr freqScale windowSize envbuf overlaps windowRandRatio interp

Warp a buffer with a time pointer

Inspired by Chad Kirby's SuperCollider2 Warp1 class, which was
inspired by Richard Karpen's sndwarp for CSound. A granular time
strecher and pitchshifter.

nc - the number of channels in the soundfile used in bufnum.

buf - the buffer number of a mono soundfile.

ptr - the position in the buffer.  The value should be between 0
      and 1, with 0 being the begining of the buffer, and 1 the
      end.

freqScale - the amount of frequency shift. 1.0 is normal, 0.5 is
            one octave down, 2.0 is one octave up. Negative values
            play the soundfile backwards.

windowSize - the size of each grain window.

envbuf - the buffer number containing a singal to use for the grain
         envelope. -1 uses a built-in Hanning envelope.

overlaps - the number of overlaping windows.

windowRandRatio - the amount of randomness to the windowing
                  function.  Must be between 0 (no randomness) to
                  1.0 (probably to random actually)

interp - the interpolation method used for pitchshifting grains. 1
         = no interpolation. 2 = linear. 4 = cubic interpolation
         (more computationally intensive).

> import Sound.SC3

> let { fn = "/home/rohan/audio/metal.wav"
>     ; p = linLin (lfSaw KR 0.05 0) (-1) 1 0 1
>     ; x = mouseX KR 0.5 2 Linear 0.1
>     ; w = warp1 1 10 p x 0.1 (-1) 8 0.1 2 }
> in withSC3 (\fd -> do { send fd (b_allocRead 10 fn 0 0)
>                       ; play fd (out 0 w) })
