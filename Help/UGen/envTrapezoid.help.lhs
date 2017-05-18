    :t Sound.SC3.envTrapezoid

> import Sound.SC3

> g_01 =
>     let t = envTrapezoid 0.05 0.95 3 0.1
>         e = envGen KR 1 1 0 1 RemoveSynth t
>     in sinOsc AR 440 0 * e

> e_02 = envTrapezoid 0 0.25 2 0.1 :: Envelope Double
> r_02 = [0,3,-99,-99,0.1,0.5,1,0,0.1,0,1,0,0,1.5,1,0]
> a_02 = envelope_sc3_array e_02

     a_02 == Just r_02

> e_03 :: [Envelope Double]
> e_03 = [envTrapezoid 0.75 0.25 2 1,envTrapezoid 0.25 0.75 3 0.5]

    import Sound.SC3.Plot
    plotEnvelope e_03
