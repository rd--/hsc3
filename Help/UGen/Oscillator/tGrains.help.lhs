tGrains numChannels trigger bufnum rate centerPos dur pan amp interp

Buffer granulator.  Triggers generate grains from a buffer. Each
grain has a Hanning envelope (sin^2(x) for x from 0 to pi) and is
panned between two channels of multiple outputs.

numChannels - number of output channels.

trigger - at each trigger, the following arguments are sampled and
          used as the arguments of a new grain.  A trigger occurs
          when a signal changes from <= 0 to > 0.  If the trigger
          is audio rate then the grains will start with sample
          accuracy.

bufnum - the index of the buffer to use. It must be a one channel
         (mono) buffer.

rate - 1.0 is normal, 2.0 is one octave up, 0.5 is one octave down
       and -1.0 is backwards normal rate ... etc.  Unlike PlayBuf,
       the rate is multiplied by BufRate, so you needn't do that
       yourself.

centerPos - the position in the buffer in seconds at which the
            grain envelope will reach maximum amplitude.

dur - duration of the grain in seconds.

pan - a value from -1 to 1. Determines where to pan the output in
      the same manner as PanAz.

amp - amplitude of the grain.

interp - 1, 2, or 4. Determines whether the grain uses (1) no
         interpolation, (2) linear interpolation, or (4) cubic
         interpolation.

> withSC3 (\fd -> async fd (b_allocRead 10 "/home/rohan/audio/metal.wav" 0 0))

> let { tRate = mouseY KR 2 200 Exponential 0.1
>     ; ctr = mouseX KR 0 (bufDur KR 10) Linear 0.1
>     ; tr = impulse AR tRate 0 }
> in audition (out 0 (tGrains 2 tr 10 1 ctr (4 / tRate) 0 0.1 2))

> let { b = 10
>     ; trate = mouseY KR 8 120 Exponential 0.1
>     ; dur = 4 / trate }
> in do { clk <- dust AR trate
>       ; r <- tRand 0 0.01 clk
>       ; pan <- return . (* 0.6) =<< whiteNoise KR
>       ; let { x = mouseX KR 0 (bufDur KR b) Linear 0.1
>             ; pos = x + r }
>         in audition (out 0 (tGrains 2 clk b 1 pos dur pan 0.1 2)) }

> let { b = 10
>     ; trate = mouseY KR 2 120 Exponential 0.1
>     ; dur = 1.2 / trate
>     ; clk = impulse AR trate 0
>     ; pos = mouseX KR 0 (bufDur KR b) Linear 0.1 }
> in do { n0 <- whiteNoise KR
>       ; n1 <- whiteNoise KR
>       ; let rate = shiftLeft 1.2 (roundE (n0 * 3) 1)
>         in audition (out 0 (tGrains 2 clk b rate pos dur (n1 * 0.6) 0.25 2)) }
