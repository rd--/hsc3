inFeedback numChannels bus

Read signal from a bus without erasing it.

The output (Out) ugens overwrite data on the bus, giving this bus a
new timestamp so that any input (In) ugen can check if the data was
written within the current cycle. The next cycle this data is still
there, but in case of audio one normally doesn't want an in ugen to
read it again, as it might cause feedback.

This is the reason why In ar checks the timestamp and ignores
everything that was not written within this cycle. This means that
nodes can only read data from a bus that was written by a
preceeding node when using the In ar ugen which overwrites the old
data. This is good for audio, but for control data it is more
convenient to be able to read a bus from any place in the node
order.

This is why In kr behaves differently and reads also data with a
timestamp that is one cycle old. Now in some cases we want to be
able to read audio from a bus independant of the current node
order, which is the use of InFeedback.  The delay introduced by
this is at a maximum one block size, which equals about 0.0014 sec
at the default block size and sample rate.

Audio feedback modulation.

> let f = inFeedback 1 0 * 1300 + 300
> audition $ sinOsc AR f 0 * 0.4

Evaluate these in either order and hear both tones.

> audition $ out 0 (inFeedback 1 10)

> let a = out 10 (sinOsc AR 220 0 * 0.1)
>     b = out 0 (sinOsc AR 660 0 * 0.1)
> audition $ MRG [a, b]

Doubters consult this.

> audition $ in' 1 AR 10

Resonator, see localOut for variant.

> let p = inFeedback 1 10
>     i = impulse AR 1 0
>     d = delayC (i + (p * 0.995)) 1 (recip 440 - recip controlRate)
> audition $ MRG [offsetOut 10 d, offsetOut 0 p]

Compare with oscillator.

> audition $ out 1 (sinOsc AR 440 0 * 0.2)
