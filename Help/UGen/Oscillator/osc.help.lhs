osc rate bufnum freq phase

Linear interpolating wavetable lookup oscillator with frequency and
phase modulation inputs.

This oscillator requires a buffer to be filled with a wavetable
format signal.  This preprocesses the Signal into a form which can
be used efficiently by the Oscillator.  The buffer size must be a
power of 2.

This can be acheived by creating a Buffer object and sending it one
of the "b_gen" messages ( sine1, sine2, sine3 ) with the wavetable
flag set to true.

Note about wavetables: OscN requires the b_gen sine1 wavetable flag
to be OFF.  Osc requires the b_gen sine1 wavetable flag to be ON.

> withSC3 (\fd -> do send fd (b_alloc 10 512 1)
>                    wait fd "/done"
>                    send fd (b_gen 10 "sine1" [1 + 2 + 4, 1, 1/2, 1/3, 1/4, 1/5]))

> audition $ osc AR 10 220 0 * 0.1

Modulate freq

> let f = xLine KR 2000 200 1 DoNothing
> audition $ osc AR 10 f 0 * 0.1

Modulate freq

> let f = osc AR 10 (xLine KR 1 1000 9 RemoveSynth) 0 * 200 + 800
> audition $ osc AR 10 f 0 * 0.1

Modulate phase

> let p = osc AR 10 (xLine KR 20 8000 10 RemoveSynth) 0 * 2 * pi
> audition $ osc AR 10 800 p * 0.1

Change the buffer while its playing

> audition $ osc AR 10 220 0 * 0.1

> r <- getStdRandom (randomR (0.0,1.0))
> withSC3 (\fd -> send fd (b_gen 10 "sine1" [1 + 2 + 4, 1, r, 1/4]))
