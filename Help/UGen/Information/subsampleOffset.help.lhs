subsampleOffset

Offset from synth start within one sample.

When a synth is created from a time stamped osc-bundle, it starts
calculation at the next possible block (normally 64 samples). Using
an OffsetOut ugen, one can delay the audio so that it matches
sample accurately.  For some synthesis methods, one needs subsample
accuracy. SubsampleOffset provides the information where, within
the current sample, the synth was scheduled. It can be used to
offset envelopes or resample the audio output.

See also OffsetOut.

Demonstrate cubic subsample interpolation.  An impulse train that can
be moved between samples.  Create two pulse trains one sample apart,
move one relative to the other.  When cursor is at the left, the
impulses are adjacent, on the right, they are exactly 1 sample apart.
View this with an oscilloscope.

> let { a = control KR "a" 0
>     ; i = impulse AR 2000 0 * 0.3
>     ; d = sampleDur
>     ; x = 4
>     ; o = (1 - subsampleOffset) + mouseX KR 0 a Linear 0.1
>     ; r = delayC i (d * (1 + x)) (d * (o + x))
>     ; g = offsetOut 0 r }
> in withSC3 (\fd -> do { send fd (d_recv (graphdef "s" (graph g)))
>                       ; wait fd "/done"
>                       ; t <- utc
>                       ; let { t' = t + 0.2
>                             ; dt = 1 / 44100.0
>                             ; m n = s_new "s" (-1) AddToTail 1 [("a", n)] }
>                         in do { send fd (Bundle t' [m 3])
>                                 ; send fd (Bundle (t' + dt) [m 0]) } })

