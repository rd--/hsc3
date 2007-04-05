degreeToKey bufnum in octave

Convert signal to modal pitch

The input signal value is truncated to an integer value and used
as an index into an octave repeating table of note values.
Indices wrap around the table and shift octaves as they do.

bufnum - index of the buffer which contains the steps for each
         scale degree.
in     - the input signal.
octave - the number of steps per octave in the scale.

> withSC3 (\fd -> do send fd (b_alloc 0 7 1)
>                    wait fd "/done"
>                    send fd (b_setn 0 [(0, [0, 2, 3.2, 5, 7, 9, 10])]))

> n <- lfNoise1 KR (MCE [3, 3.05])
> let x = mouseX KR 0 15 Linear 0.1
>     k = degreeToKey 0 x 12
>     f b = combN m 0.31 0.31 2 + m
>         where o = sinOsc AR (midiCPS (b + k + n * 0.04)) 0 * 0.1
>               t = lfPulse AR (midiCPS (MCE [48, 55])) 0.15 0.5
>               d = rlpf t (midiCPS (sinOsc KR 0.1 0 * 10 + b)) 0.1 * 0.1
>               m = o + d
> audition (out 0 ((f 48 + f 72) * 0.25))
