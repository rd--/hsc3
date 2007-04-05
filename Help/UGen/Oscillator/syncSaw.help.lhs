syncSaw rate syncFreq sawFreq

A sawtooth wave that is hard synched to a fundamental pitch. This
produces an effect similar to moving formants or pulse width
modulation. The sawtooth oscillator has its phase reset when the
sync oscillator completes a cycle. This is not a band limited
waveform, so it may alias.

The frequency of the slave synched sawtooth wave should always be
greater than the syncFreq.

> audition (out 0 (syncSaw AR 100 (line KR 100 800 12 RemoveSynth) * 0.1))
