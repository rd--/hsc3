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

> let sync fd msg = send fd msg >> wait fd "/done"
> withSC3 (\fd -> sync fd (b_allocRead 10 "/home/rohan/sw/sw-01/audio/metal.wav" 0 0))

> let tRate = mouseY KR 2 200 Exponential 0.1
> let ctr   = mouseX KR 0 (bufDur KR 10) Linear 0.1
> let tr    = impulse AR tRate 0
> audition $ tGrains 2 tr 10 1 ctr (4 / tRate) 0 0.1 2
