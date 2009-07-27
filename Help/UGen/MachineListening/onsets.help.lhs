onsets c threshold odftype relaxtime floor mingap medianspan whtype rawodf
onsets' c threshold odftype

An onset detector for musical audio signals - detects the
beginning of notes/drumbeats/etc. Outputs a control-rate
trigger signal which is 1 when an onset is detected, and 0
otherwise.

          c - an FFT chain

  threshold - the detection threshold, typically between 0
              and 1, although in rare cases you may find
              values outside this range useful

    odftype - the function used to analyse the signal
              (options described below; OK to leave this at
              its default value)

  relaxtime - specifies the time (in seconds) for the
              normalisation to "forget" about a recent
              onset. If you find too much re-triggering
              (e.g. as a note dies away unevenly) then you
              might wish to increase this value.

      floor - is a lower limit, connected to the idea of how
              quiet the sound is expected to get without
              becoming indistinguishable from noise. For
              some cleanly-recorded classical music with
              wide dynamic variations, I found it helpful to
              go down as far as 0.000001.

     mingap - specifies a minimum gap (in seconds) between
              onset detections, a brute-force way to prevent
              too many doubled detections.

 medianspan - specifies the size (in FFT frames) of the
              median window used for smoothing the detection
              function before triggering.

For the FFT chain, you should typically use a frame size of
512 or 1024 (at 44.1 kHz sampling rate) and 50% hop size
(which is the default setting in SC). For different sampling
rates choose an FFT size to cover a similar time-span
(around 10 to 20 ms).

The onset detection should work well for a general range of
monophonic and polyphonic audio signals. The onset detection
is purely based on signal analysis and does not make use of
any "top-down" inferences such as tempo.

> import Sound.SC3

> let { x = mouseX KR 0 1 Linear 0.2
>     ; i = soundIn 0
>     ; c = fft' 10 i
>     ; o = onsets' c x (onsetType "rcomplex")
>     ; s = sinOsc AR 440 0 * 0.2
>     ; e = envGen KR o 1 0 1 DoNothing (envPerc 0.001 0.1) }
> in withSC3 (\fd -> do { async fd (b_alloc 10 512 1)
>                       ; play fd (out 0 (s * e)) })

The type argument chooses which onset detection function is
used. In many cases the default will be fine. The following
choices are available:

    power - generally OK, good for percussive input, and
            also very efficient

   magsum - generally OK, good for percussive input, and
            also very efficient

  complex - performs generally very well, but more
            CPU-intensive

 rcomplex - performs generally very well, and slightly more
            efficient than complex

    phase - generally good, especially for tonal input,
            medium efficiency

   wphase - generally very good, especially for tonal input,
            medium efficiency

      mkl - generally very good, medium efficiency, pretty
            different from the other methods

Which of these should you choose? The differences aren't
large, so I'd recommend you stick with the default \rcomplex
unless you find specific problems with it. Then maybe try
\wphase. The \mkl type is a bit different from the others so
maybe try that too. They all have slightly different
characteristics, and in tests perform at a similar quality
level.

For more details of all the processes involved, the
different onset detection functions, and their evaluation,
see

D. Stowell and M. D. Plumbley. Adaptive whitening for
improved real-time audio onset detection. Proceedings of the
International Computer Music Conference (ICMC’07),
Copenhagen, Denmark, August 2007.
