decay2 in attackTime decayTime

Exponential decay.  Decay has a very sharp attack and can produce
clicks.  Decay2 rounds off the attack by subtracting one Decay from
another.

decay2 AR i a d is equivalent to decay AR i d - decay AR i a

Used as an envelope

> let s = fSinOsc AR 600 0 * 0.25
>     f = xLine KR 1 50 20 RemoveSynth
> audition (out 0 (decay2 (impulse AR f 0.25) 0.01 0.2 * s))

Compare the above with Decay used as the envelope.

> let s = fSinOsc AR 600 0 * 0.25
>     f = xLine KR 1 50 20 RemoveSynth
> audition (out 0 (decay (impulse AR f 0.25) 0.2 * s))
