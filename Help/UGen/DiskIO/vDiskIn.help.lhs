vDiskIn nc b r l

  nc - number of channels
   b - buffer number
   r - playback rate
   l - loop indicator

Continously play a soundfile from disk with variable playback
rate. This requires a buffer to be preloaded with one buffer size of
sound.  The buffer size must be a multiple of twice the synth block
size. The default block size is 64.

> import Sound.SC3

> let { f = "/home/rohan/audio/metal.wav"
>     ; n = 1
>     ; g = out 0 (vDiskIn n 0 (sinOsc KR 0.25 0 * 0.25 + 1) Loop) }
> in withSC3 (\fd -> do { async fd (b_alloc 0 8192 n)
>                       ; async fd (b_read 0 f 0 (-1) 0 1)
>                       ; play fd g })

> withSC3 (\fd -> do { reset fd
>                    ; async fd (b_close 0)
>                    ; async fd (b_free 0) })
