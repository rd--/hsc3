> Sound.SC3.UGen.Help.viewSC3Help "SubsampleOffset"
> Sound.SC3.UGen.DB.ugenSummary "SubsampleOffset"

> import Sound.OSC
> import Sound.SC3

Impulse train that can be moved between samples
> let g = let {a = control KR "a" 0
>             ;i = impulse AR 2000 0 * 0.3
>             ;d = sampleDur
>             ;x = 4
>             ;o = (1 - subsampleOffset) + mouseX KR 0 a Linear 0.1
>             ;r = delayC i (d * (1 + x)) (d * (o + x))}
>         in (synthdef "s" (offsetOut 0 r))

Create two pulse trains one sample apart, move one relative to the
other.  When cursor is at the left, the impulses are adjacent, on the
right, they are exactly 1 sample apart.  View this with an
oscilloscope.
> let run s = do
>       {_ <- async (d_recv s)
>       ;t <- time
>       ;sr <- serverSampleRateActual
>       ;let {t' = t + 0.2
>            ;dt = 1 / sr
>            ;m n = s_new "s" (-1) AddToTail 1 [("a", n)]}
>        in do {sendBundle (bundle t' [m 3])
>              ;sendBundle (bundle (t' + dt) [m 0]) }}

> withSC3 (run g)
