    > Sound.SC3.UGen.Help.viewSC3Help "OffsetOut"
    > Sound.SC3.UGen.DB.ugenSummary "OffsetOut"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let a = offsetOut 0 (impulse AR 5 0)
>         b = out 0 (sinOsc AR 60 0 * 0.1)
>     in mrg [a,b]
>
> g_02 =
>     let a = out 0 (impulse AR 5 0)
>         b = out 0 (sinOsc AR 60 0 * 0.1)
>     in mrg [a,b]

Phase cancellation, the `offsetOut` at bus 0 should cancel, the `out` at
bus 1 doesn't (or at least is exceedingly unlikely to).

> sy_01 sr =
>     let f = sr / 100
>         o = sinOsc AR (constant f) 0 * 0.2
>     in synthdef "sy_01" (mrg [offsetOut 0 o,out 1 o])
>
> bnd_01 sr t =
>     let latency = 0.2
>         c = 100 / sr {- recip f -}
>         m = s_new "sy_01" (-1) AddToHead 1 []
>         p = bundle (t + latency) [m]
>         q = bundle (t + latency + c/2) [m]
>     in [p,q]
>
> proc_01 :: Transport m => m ()
> proc_01 = do
>   sr <- serverSampleRateActual
>   _ <- async (d_recv (sy_01 sr))
>   t <- time
>   mapM_ sendBundle (bnd_01 sr t)

    > withSC3 proc_01

The cancellation isn't completely reliable though.  In supercollider language it seems
to be better, though these _should_ be equivalent...

    Routine(
    {var sr = s.actualSampleRate
    ;var f = sr / 100
    ;var c = 1 / f
    ;var g = {var o = SinOsc.ar(f,0) * 0.2; OffsetOut.ar(0,o); Out.ar(1,o)}
    ;var sy = SynthDef("g",g)
    ;var m = ["/s_new", "g", -1, 0, 1]
    ;var latency = 0.2
    ;sy.send(s)
    ;s.sync
    ;s.sendBundle(latency,m)
    ;s.sendBundle(latency + (c/2),m)}).play

To see/hear scheduler clock and sample clock drift:

    Routine(
    {var g = {OffsetOut.ar(0,Saw.ar(400) * EnvGen.kr(Env.perc, doneAction: 2) * 0.2)}
    ;var h = {OffsetOut.ar(1,Saw.ar(800) * Decay2.ar(Impulse.ar(1)) * 0.2)}
    ;var t_delay = 0.25
    ;SynthDef("g",g).send(s)
    ;SynthDef("h",h).send(s)
    ;s.sync
    ;s.sendBundle(t_delay,["/s_new", "h", -1, 0, 1])
    ;inf.do({s.sendBundle(t_delay,["/s_new", "g", -1, 0, 1]); 1.0.wait})}).play

