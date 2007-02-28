tDuty rate duration reset doneAction level

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

Play a little rhythm

> d <- dseq 8192 (MCE [0.1, 0.2, 0.4, 0.3])
> audition $ tDuty AR d 0 DoNothing 1

Amplitude changes

> d0 <- dseq 8192 (MCE [0.1, 0.2, 0.4, 0.3])
> d1 <- dseq 8192 (MCE [0.1, 0.4, 0.01, 0.5, 1.0])
> audition $ ringz (tDuty AR d0 0 DoNothing d1) 1000 0.1

Mouse control.

> d <- dseq 8192 (MCE [0.1, 0.4, 0.01, 0.5, 1.0])
> let x = mouseX KR 0.001 1 Linear 0.1
> audition $ ringz (tDuty AR x 0 DoNothing d) 1000 0.1
