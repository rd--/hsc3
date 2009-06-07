blip rate freq numHarm

Band Limited ImPulse generator. All harmonics have equal amplitude.

This is the equivalent of 'buzz' in MusicN languages. WARNING: This
waveform in its raw form could be damaging to your ears at high
amplitudes or for long periods.

Implementation notes: It is improved from other implementations in
that it will crossfade in a control period when the number of
harmonics changes, so that there are no audible pops. It also
eliminates the divide in the formula by using a 1/sin table (with
special precautions taken for 1/0).  The lookup tables are linearly
interpolated for better quality.

The number of harmonics may be lowered internally if it would cause
aliasing.

> import Sound.SC3

> audition (out 0 (blip AR 440 200 * 0.1))

Modulate frequency

> let f = xLine KR 20000 200 6 RemoveSynth
> in audition (out 0 (blip AR f 100 * 0.1))

Modulate number of harmonics.

> let nh = line KR 1 100 20 RemoveSynth
> in audition (out 0 (blip AR 200 nh * 0.2))
