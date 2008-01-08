vOsc rate bufpos freq phase

Variable wavetable oscillator.  A wavetable lookup oscillator which
can be swept smoothly across wavetables. All the wavetables must be
allocated to the same size. Fractional values of table will
interpolate between two adjacent tables.

This oscillator requires a buffer to be filled with a wavetable
format signal.  This preprocesses the Signal into a form which can
be used efficiently by the Oscillator.  The buffer size must be a
power of 2.

This can be acheived by creating a Buffer object and sending it one
of the "b_gen" messages (sine1, sine2, sine3) with the wavetable
flag set to true.

This can also be acheived by creating a Signal object and sending
it the 'asWavetable' message, saving it to disk, and having the
server load it from there.

Note about wavetables: VOsc requires the b_gen sine1 wavetable flag
to be ON.

Allocate and fill tables 0 to 7.

> let square a = a * a
>     harmonics i = map f [0 .. n - 1]
>         where n = square (i + 1)
>               f j = square ((n - j) / n)
>     setup fd i = do let i' = fromIntegral i
>                     send fd (b_alloc i 1024 1)
>                     wait fd "/done"
>                     send fd (b_gen i "sine1" (1 + 2 + 4 : harmonics i'))
> withSC3 (\fd -> mapM_ (setup fd) [0 .. 7])

Oscillator at buffers 0 through 7, mouse selects buffer.

> let x = mouseX KR 0 7 Linear 0.1
> audition (out 0 (vOsc AR x (mce [120, 121]) 0 * 0.3))

Reallocate buffers while oscillator is running.

> let rrand l r = getStdRandom (randomR (l,r))
>     rrandl n l r = replicateM n (rrand l r)
>     resetTable fd i = do h <- rrandl 12 0 1
>                          send fd (b_gen i "sine1" (1 + 2 + 4 : h))
> withSC3 (\fd -> mapM_ (resetTable fd) [0 .. 7])
