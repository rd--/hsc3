beatTrack c lock

     c - Audio input to track, already passed through an FFT
         UGen; the expected size of FFT is 1024 for 44100
         and 48000 sampling rate, and 2048 for double
         those. No other sampling rates are supported.

  lock - If this argument is greater than 0.5, the tracker
         will lock at its current periodicity and continue
         from the current phase. Whilst it updates the
         model's phase and period, this is not reflected in
         the output until lock goes back below 0.5.

Autocorrelation based beat tracker; the underlying model
assumes 4/4, but it should work on any isochronous beat
structure, though there are biases to 100-120 bpm; a fast
7/8 may not be tracked in that sense. There are four k-rate
outputs, being ticks at quarter, eighth and sixteenth level
from the determined beat, and the current detected
tempo. Note that the sixteenth note output won't necessarily
make much sense if the music being tracked has swing; it is
provided just as a convenience.

This beat tracker determines the beat, biased to the
midtempo range by weighting functions. It does not determine
the measure level, only a tactus. It is also slow reacting,
using a 6 second temporal window for its autocorrelation
maneouvres. Don't expect human musician level predictive
tracking.

On the other hand, it is tireless, relatively general
(though obviously best at transient 4/4 heavy material
without much expressive tempo variation), and can form the
basis of computer processing that is decidedly faster than
human.

> import Sound.SC3

> let { i = soundIn 0
>     ; x = mouseX KR (-1) 1 Linear 0.2
>     ; MCE [b, h, q, t] = beatTrack (fft' 10 i) x
>     ; f = mce [440, 660, 880]
>     ; a = mce [0.4, 0.2, 0.1]
>     ; s = mix (sinOsc AR f 0 * a * decay (mce [b, h, q]) 0.05) }
> in withSC3 (\fd -> do { async fd (b_alloc 10 1024 1)
>                       ; play fd (out 0 (i + s)) })

Davies, M. E. P.  and Plumbley, M. D. Beat Tracking With A
Two State Model. Proceedings of the IEEE International
Conference on Acoustics, Speech and Signal Processing
(ICASSP 2005), Philadelphia, USA, March 19-23, 2005

The UGen was converted by Nick Collins for beat tracking
research in the course of his PhD and uses an original C
implementation of Matthew Davies' MATLAB model. It first
appeared as part of BBCut2 as AutoTrack but has now been
added to core to enhance SuperCollider's realtime machine
listening options.


