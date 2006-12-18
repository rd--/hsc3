amplitude rate in attackTime releaseTime

Amplitude follower. Tracks the peak amplitude of a signal.

> let s = in' 1 AR numOutputBuses
> audition $ pulse AR 90 0.3 * amplitude KR s 0.1 0.1

> let s = in' 1 AR numOutputBuses
> audition $ sinOsc AR (amplitude KR s 0.1 0.1 * 1200 + 400) 0 * 0.3
