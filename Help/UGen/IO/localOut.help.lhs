localOut signal

Write to buses local to a synth.

LocalOut writes to buses that are local to the enclosing synth. The
buses should have been defined by a LocalIn ugen. The channelsArray
must be the same number of channels as were declared in the
LocalIn. These are like the global buses, but are more convenient if
you want to implement a self contained effect that uses a feedback
processing loop.  See [LocalIn].

N.B. Audio written to a LocalOut will not be read by a corresponding
LocalIn until the next cycle, i.e. one block size of samples
later. Because of this it is important to take this additional delay
into account when using LocalIn to create feedback delays with delay
times shorter than the threshold of pitch (i.e. < 0.05 seconds or >
20Hz), or where sample accurate alignment is required. See the
resonator example below.

> n <- whiteNoise AR
> let a0 = decay (impulse AR 0.3 0) 0.1 * n * 0.2
>     a1 = localIn 2 AR + MCE [a0, 0]
>     a2 = delayN a1 0.2 0.2
> audition (mrg [localOut (mceReverse a2 * 0.8), out 0 a2])

Resonator, must subtract blockSize for correct tuning

> let p = localIn 1 AR
>     i = impulse AR 1 0
>     d = delayC (i + (p * 0.995)) 1 (recip 440 - recip controlRate)
> audition (mrg [offsetOut 0 p, localOut d])

Compare with oscillator.

> audition (out 1 (sinOsc AR 440 0 * 0.2))
