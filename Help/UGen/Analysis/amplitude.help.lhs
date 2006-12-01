amplitude in attackTime releaseTime

Amplitude follower. Tracks the peak amplitude of a signal.

> let s = in' 1 AR numInputBuses
> audition $ pulse AR 90 0.3 * amplitude KR s 0.01 0.01

> let s = in' 1 AR numInputBuses
> audition $ sinOsc AR (amplitude KR s 0.01 0.01 * 1200 + 400) 0 * 0.3
