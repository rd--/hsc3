tDuty rate duration reset doneAction level gap

Demand results as trigger from demand rate ugens.

A value is demanded each ugen in the list and output as a trigger
according to a stream of duration values.  The unit generators in
the list should be 'demand' rate.  When there is a trigger at the
reset input, the demand rate ugens in the list and the duration are
reset.  The reset input may also be a demand ugen, providing a
stream of reset times.

duration   - time values. Can be a demand ugen or any signal.
             The next trigger value is acquired after the
             duration provided by the last time value.

reset      - trigger or reset time values. Resets the list of ugens
             and the duration ugen when triggered. The reset input
             may also be a demand ugen, providing a stream of reset
             times.

doneAction - a doneAction that is evaluated when the duration
             stream ends.

level      - demand ugen providing the output values.

gap - if true the dirst duration precedes the first level,
      else it follows it.

Play a little rhythm

> import Sound.SC3

> do { d <- dseq dinf (mce [0.1, 0.2, 0.4, 0.3])
>    ; audition (out 0 (tDuty AR d 0 DoNothing 1 1)) }

Amplitude changes

> do { d0 <- dseq dinf (mce [0.1, 0.2, 0.4, 0.3])
>    ; d1 <- dseq dinf (mce [0.1, 0.4, 0.01, 0.5, 1.0])
>    ; let s = ringz (tDuty AR d0 0 DoNothing d1 1) 1000 0.1
>      in audition (out 0 s) }

Mouse control.

> do { d <- dseq dinf (mce [0.1, 0.4, 0.01, 0.5, 1.0])
>    ; let { x = mouseX KR 0.003 1 Exponential 0.1
>          ; s = ringz (tDuty AR x 0 DoNothing d 1) 1000 0.1 }
>      in audition (out 0 s) }

Note that the 440 is the shorter pitch, since gap is set to false

> do { d0 <- dser 12 (mce [0.1, 0.3])
>    ; d1 <- dser 12 (mce [440, 880])
>    ; let t = tDuty AR d0 0 RemoveSynth d1 0
>      in audition (out 0 (sinOsc AR (latch t t) 0 * 0.1)) }
